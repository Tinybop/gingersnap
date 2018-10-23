{-# LANGUAGE
     ExistentialQuantification
   , InstanceSigs
   , LambdaCase
   , OverloadedStrings
   , ViewPatterns
   #-}

module Gingersnap.Core (
     IsCtx(..)
   , ApiErr(..)
   , ErrResult(..)

   , Rsp(..)

   , rspGood
   , rspBad
   , rspBadCommit
   , rspBadRollback
   , rspGoodCSV
   , rspGoodLBS
   , rspEmptyGood

   , pureRsp

   , inTransaction
   , inTransaction_readOnly
   , inTransaction_override
   , inTransactionMode
   , rspIsGood

   , errorEarlyCode

   -- These won't typically be inspected by hand but there's no reason we should
   --   block people from inspecting them if they like
   , RspPayload(..)
   , ShouldCommitOrRollback(..)

   -- Maybe?:
   -- , module import Network.HTTP.Types.Status
   -- , module Snap.Core
   ) where

import qualified Control.Exception as E
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON(..), (.=))
import qualified Data.Aeson as JSON
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import Data.Pool (Pool)
import qualified Data.Pool as Pool
import Database.PostgreSQL.Simple (Connection)
import qualified Database.PostgreSQL.Simple as PSQL
import qualified Database.PostgreSQL.Simple.Transaction as PSQL
import qualified Network.HTTP.Types.Status as HTTP
import Snap.Core (Snap)
import qualified Snap.Core as Snap

class IsCtx ctx where
   ctxConnectionPool :: ctx -> Pool Connection

   ctxGetReadOnlyMode :: ctx -> IO Bool
   ctxGetReadOnlyMode _ = pure False

   ctx_wrapSuccess :: ToJSON x => ctx -> x -> JSON.Value
   ctx_wrapSuccess _ x = JSON.object ["result" .= x]

   ctx_err_inReadOnlyMode :: ctx -> ErrResult
   ctx_err_inReadOnlyMode _ = errResult DefaultErrors_ReadOnlyMode

class ApiErr apiErr where
   errResult :: apiErr -> ErrResult

data ErrResult
   = ErrResult HTTP.Status JSON.Value
 deriving (Show, Eq)

-- Might be nice, since we don't have a Read.
-- But probably better for a 'pretty*' function
{-
instance Show ErrResult where
   show (ErrResult status j) =
      "ErrResult "++show status++" "++show (JSON.encode j)
-}

instance ApiErr ErrResult where
   errResult x = x

data DefaultErrors
   = DefaultErrors_ReadOnlyMode
 deriving (Show, Eq)

instance ApiErr DefaultErrors where
   errResult :: DefaultErrors -> ErrResult
   errResult = \case
      DefaultErrors_ReadOnlyMode -> ErrResult HTTP.serviceUnavailable503 $
         JSON.object [
              "errorCode" .= (0 :: Int)
            , "errorMessage" .= JSON.String "This action is unavailable in read-only mode"
            ]

data Rsp
   = Rsp {
     rspShouldCommit :: ShouldCommitOrRollback
   , rspPayload :: RspPayload
   }

data RspPayload
   = forall x. ToJSON x => RspPayload_Good x
   | forall e. ApiErr e => RspPayload_Bad e
   -- | First ByteString is MIME type; second is response body
   | RspPayload_Custom HTTP.Status BS.ByteString BSL.ByteString
   | RspPayload_Empty -- This might be a dupe with '_Custom' but it's nice to have

data ShouldCommitOrRollback
   = ShouldCommit
   | ShouldRollback
 deriving (Show, Eq)

-- | This means everything's succeeded. We should commit DB changes and
--   return a success object
rspGood :: ToJSON x => x -> Rsp
rspGood x = Rsp ShouldCommit $ RspPayload_Good x

rspBad, rspBadCommit, rspBadRollback :: ApiErr ae => ae -> Rsp
-- | We should send back an error object and roll back DB changes
rspBad e       = Rsp ShouldRollback $ RspPayload_Bad e
-- | Like 'rspBad' but should still commit DB changes
rspBadCommit e = Rsp ShouldCommit   $ RspPayload_Bad e
-- | The same as 'rspBad' but more explicit that we roll back
rspBadRollback e = Rsp ShouldRollback $ RspPayload_Bad e

rspGoodCSV :: BSL.ByteString -> Rsp
rspGoodCSV bs = Rsp ShouldCommit $ RspPayload_Custom HTTP.ok200 (BS8.pack "text/csv") bs

-- | First Bytestring is the content type, e.g. "application/json"
--   Here's a helpful list:
--   https://developer.mozilla.org/en-US/docs/Web/HTTP/Basics_of_HTTP/MIME_types/Complete_list_of_MIME_types
rspGoodLBS :: BS.ByteString -> BSL.ByteString -> Rsp
rspGoodLBS mimeType bs = Rsp ShouldCommit $ RspPayload_Custom HTTP.ok200 mimeType bs

{-
rspCustomLBS :: ShouldCommitOrRollback -> HTTP.Status -> BS.ByteString -> BSL.ByteString -> Rsp
rspCustomLBS shouldCommit status mimeType bs =
   Rsp shouldCommit $ RspPayload_Custom status mimeType bs
-}

-- | Everything worked and we send a 200, but we don't have any data to send
rspEmptyGood :: Rsp
rspEmptyGood = Rsp ShouldCommit RspPayload_Empty

-- Extra helpers we could add:
{-
data Rsp
   -- | Like 'RspGood' but rolls back. Sure, why not? Maybe we'll want this for
   --   something...
   | forall x. ToJSON x => RspGoodRollback x

   -- | We use this in the case where we want to rollback but don't want to tell
   --     the user about it. E.g. if we want to not create an account because
   --     the email is already taken - but we don't want to tell the
   --     unauthenticated user that that email is taken (because it's leaking
   --     information about our users)
   | RspEmptyGoodRollback
-}

rspIsGood :: Rsp -> Bool
rspIsGood (Rsp _ payload) = case payload of
   RspPayload_Good {} -> True
   RspPayload_Bad {} -> False
   RspPayload_Custom httpStatus _ _ -> httpStatus == HTTP.ok200
   RspPayload_Empty -> True

instance Show Rsp where
   show (Rsp commit payload) =
      "Rsp "++show commit++" "++case payload of
         RspPayload_Good x -> "(Good "++show (JSON.encode x)++")"
         RspPayload_Bad (errResult -> e) -> "(Bad "++show e++")"
         RspPayload_Empty -> "Empty"
         RspPayload_Custom a b c -> "(Custom "++show (a,b,c)++")"

-- | *If you hit the DB, use this function!*
-- 
--   This is a lot like 'withTransaction', but it allows us to rollback if we
--   want, without throwing an error.
--   (Don't use 'withTransaction'!)
-- 
--   NOTE this is for IO actions, not Snap actions. This is to ensure we can't
--   call e.g. 'finishEarly' and never hit the 'end transaction' code!
--   (It also has the side benefit of keeping code fairly framework-agnostic)
inTransaction :: IsCtx ctx => ctx -> (Connection -> IO Rsp) -> Snap ()
inTransaction ctx actionThatReturnsAnRsp = do
   inTransactionMode ctx PSQL.Serializable PSQL.ReadWrite actionThatReturnsAnRsp

-- | An endpoint that uses 'inTransaction_readOnly' will keep responding even
--   when the server is in read-only mode.
-- 
--   Note that you the programmer are asserting the DB queries are read-only.
--   There's nothing in this library or in postgresql-simple which statically
--   checks that to be true!
inTransaction_readOnly :: IsCtx ctx => ctx -> (Connection -> IO Rsp) -> Snap ()
inTransaction_readOnly ctx f =
   inTransactionMode ctx PSQL.Serializable PSQL.ReadOnly f

-- | YOU SHOULD ONLY USE THIS ONCE
-- 
--   This lets you do a write transaction during read-only mode (not a
--     read-only transaction! A time where 'ctxGetReadOnlyMode' would return
--     True)
-- 
--   You may need this so that an admin user can take the app out of read-only
--     mode
inTransaction_override :: IsCtx ctx => ctx -> (Connection -> IO Rsp) -> Snap ()
inTransaction_override ctx action =
   inTransaction_internal ctx PSQL.Serializable PSQL.ReadWrite action

inTransactionMode :: IsCtx ctx => ctx -> PSQL.IsolationLevel -> PSQL.ReadWriteMode -> (Connection -> IO Rsp) -> Snap ()
inTransactionMode ctx isolationLevel' readWriteMode' actionThatReturnsAResponse = do
   readOnlyMode <- liftIO $ ctxGetReadOnlyMode ctx
   when (readOnlyMode && (readWriteMode' /= PSQL.ReadOnly)) $
      errorEarlyCode $ ctx_err_inReadOnlyMode ctx

   inTransaction_internal ctx isolationLevel' readWriteMode' actionThatReturnsAResponse

-- | DON'T USE THIS FUNCTION! This should only be called by
--   'inTransaction_override' and 'inTransactionMode'
inTransaction_internal :: IsCtx ctx => ctx -> PSQL.IsolationLevel -> PSQL.ReadWriteMode -> (Connection -> IO Rsp) -> Snap ()
inTransaction_internal ctx isolationLevel' readWriteMode' actionThatReturnsAResponse = do

   let transactMode = PSQL.TransactionMode isolationLevel' readWriteMode'
   rsp <- liftIO $ Pool.withResource (ctxConnectionPool ctx) $ \conn ->
      E.mask $ \restore -> do
         PSQL.beginMode transactMode conn
         r <- restore (actionThatReturnsAResponse conn)
            `E.onException` rollback_ conn
         (case rspShouldCommit r of
            ShouldCommit -> PSQL.commit conn
            -- Note it is safe to call rollback on a read-only transaction:
            -- https://www.postgresql.org/message-id/26036.1114469591%40sss.pgh.pa.us
            ShouldRollback -> rollback_ conn
            ) `E.onException` rollback_ conn -- To be safe. E.g. what if inspecting 'r' errors?
         pure r
   pureRsp ctx rsp

-- | Sometimes you don't need a DB connection at all!
pureRsp :: IsCtx ctx => ctx -> Rsp -> Snap ()
pureRsp ctx (Rsp _ payload) = case payload of
   RspPayload_Empty -> Snap.writeBS ""
   RspPayload_Good v -> writeJSON $ ctx_wrapSuccess ctx v
   RspPayload_Bad e -> writeApiErr e
   RspPayload_Custom httpStatus mimeType bs -> do
      Snap.modifyResponse $ Snap.setResponseCode $
         HTTP.statusCode httpStatus
      writeLBSSuccess_dontUseThis mimeType bs

-- Take a look at how postgresql-simple does it:
rollback_ :: Connection -> IO ()
rollback_ conn =
   PSQL.rollback conn `E.catch` ((\_ -> return ()) :: IOError -> IO ())

writeLBSSuccess_dontUseThis :: BS.ByteString -> BSL.ByteString -> Snap ()
writeLBSSuccess_dontUseThis contentType b = do
   Snap.modifyResponse $ Snap.setHeader "Content-Type" contentType
   Snap.writeLBS b

writeJSON :: ToJSON x => x -> Snap ()
writeJSON x = do
   Snap.modifyResponse $ Snap.setHeader "Content-Type" "application/json"
   Snap.writeLBS $ JSON.encode $ x

-- | NOTE: be very careful to not use this with any setup/teardown block like 'withTransaction'
--      - causes resource leaks
--      - BUT! This should never happen to you because all your DB code should
--        use 'inTransaction'!
-- 
--   NOTE: use 403 forbidden instead of unauthorized - unauth means not logged in at all
--
--   Also note this returns any 'Snap x' so you can use it like a throw anywhere
--     in your snap code
errorEarlyCode :: ApiErr ae => ae -> Snap x
errorEarlyCode err = do
   writeApiErr err
   Snap.getResponse >>= Snap.finishWith
-- Difference with 'pureRsp . rspBad' is that it actually 'finishWith's

writeApiErr :: ApiErr ae => ae -> Snap ()
writeApiErr (errResult -> (ErrResult httpStatus responseVal)) = do
   Snap.modifyResponse $ Snap.setResponseCode $
      HTTP.statusCode httpStatus
   writeJSON $ toJSON responseVal
