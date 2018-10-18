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

   , Rsp
   , rspGood
   , rspBad
   , rspGoodCSV
   , rspGoodLBS
   , rspEmptyGood
   , rspBadCommit

   , pureRsp

   , inTransaction
   , inTransaction_readOnly
   , inTransaction_override
   , inTransactionMode
   , rspIsGood
   , rspWillCommit

   , errorEarlyCode
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
import Database.PostgreSQL.Simple.Transaction as PSQL
import Network.HTTP.Types.Status as HTTP
import Snap.Core as Snap

class IsCtx ctx where
   ctxConnectionPool :: ctx -> Pool Connection

   ctxGetReadOnlyMode :: ctx -> IO Bool

   ctx_wrapSuccess :: ToJSON x => ctx -> x -> JSON.Value
   ctx_wrapSuccess _ x = JSON.object ["result" .= x]

   ctx_err_inReadOnlyMode :: ctx -> ErrResult
   ctx_err_inReadOnlyMode _ = errResult DefaultErrors_ReadOnlyMode

class ApiErr apiErr where
   errResult :: apiErr -> ErrResult

data ErrResult
   = ErrResult HTTP.Status JSON.Value
 deriving (Show, Eq)

instance ApiErr ErrResult where
   errResult x = x

data DefaultErrors
   = DefaultErrors_ReadOnlyMode
 deriving (Show, Eq)

instance ApiErr DefaultErrors where
   errResult :: DefaultErrors -> ErrResult
   errResult = \case
      DefaultErrors_ReadOnlyMode -> ErrResult serviceUnavailable503 $
         JSON.object [
              "errorCode" .= (0 :: Int)
            , "errorMessage" .= JSON.String "This action is unavailable in read-only mode"
            ]

-- | Responses
-- 
--   Don't use these directly! They'll soon be compressed into a few general-
--     purpose constructors
data Rsp
   -- This is an existential so that 'Rsp' doesn't need a type param, meaning you
   -- don't need to give a signature to 'RspBad':

   -- | This means everything's succeeded. We should commit DB changes and
   --   return a success object
   = forall x. ToJSON x => RspGood x

   -- | First Bytestring is the content type, e.g. "application/json"
   --   Here's a helpful list:
   --   https://developer.mozilla.org/en-US/docs/Web/HTTP/Basics_of_HTTP/MIME_types/Complete_list_of_MIME_types
   | RspGoodLBS BS.ByteString BSL.ByteString

   -- | We should send back an error object and roll back DB changes
   | forall ae. ApiErr ae => RspBad ae

   -- | Like 'RspBad' but should still commit DB changes
   | forall ae. ApiErr ae => RspBadCommit ae

   -- | Like 'RspGood' but rolls back. Sure, why not? Maybe we'll want this for
   --   something...
   | forall x. ToJSON x => RspGoodRollback x

   -- | Everything worked and we send a 200, but we don't have any data to send
   | RspEmptyGood

   -- | We use this in the case where we want to rollback but don't want to tell
   --     the user about it. E.g. if we want to not create an account because
   --     the email is already taken - but we don't want to tell the
   --     unauthenticated user that that email is taken (because it's leaking
   --     information about our users)
   | RspEmptyGoodRollback

data ShouldCommitOrRollback
   = ShouldCommit
   | ShouldRollback
 deriving (Show, Eq)

rspGood :: ToJSON x => x -> Rsp
rspGood = RspGood
rspBad :: ApiErr ae => ae -> Rsp
rspBad = RspBad
rspGoodCSV = RspGoodLBS (BS8.pack "text/csv")
rspGoodLBS = RspGoodLBS
rspEmptyGood = RspEmptyGood
rspBadCommit :: ApiErr ae => ae -> Rsp
rspBadCommit = RspBadCommit

rspIsGood :: Rsp -> Bool
rspIsGood = \case
   RspGood {} -> True
   RspGoodLBS {} -> True
   RspBad {} -> False
   RspGoodRollback {} -> True
   RspBadCommit {} -> False
   RspEmptyGood -> True
   RspEmptyGoodRollback -> False -- Note!

rspWillCommit :: Rsp -> ShouldCommitOrRollback
rspWillCommit = \case
   RspGood {} -> ShouldCommit
   RspGoodLBS {} -> ShouldCommit
   RspBad {} -> ShouldRollback
   RspGoodRollback {} -> ShouldRollback
   RspBadCommit {} -> ShouldCommit
   RspEmptyGood -> ShouldCommit
   RspEmptyGoodRollback -> ShouldRollback

-- TODO: this will change very shortly
instance Show Rsp where
   show = \case
      -- TODO: finish this up:
      RspGood x -> "(RspGood: "++show (JSON.encode x)++")"
      RspGoodLBS a b -> "(RspGoodCSV: "++show (a, b)++")"
      RspBad {} -> "RspBad" {- (errResult -> (status, toJSON -> ae))->
         "(RspBad: "++show status++show (JSON.encode ae)++")" -}
      RspBadCommit {} -> "RspBadCommit" {- (errResult -> (status, toJSON -> ae)) ->
         "(RspBadCommit: "++show status++show (JSON.encode ae)++")" -}
      RspGoodRollback {} -> "RspGoodRollback"
      RspEmptyGood -> "RspEmptyGood"
      RspEmptyGoodRollback -> "RspEmptyGoodRollback"


-- | *If you hit the DB, use this function!*
-- 
--   This is a lot like 'withTransaction', but it allows us to rollback if we
--   want, without throwing an error.
--   (Don't use 'withTransaction'!)
-- 
--   NOTE this is for IO actions, not Snap actions. This is to ensure we can't
--   call e.g. 'finishEarly' and never hit the 'end transaction' code!
--   (It also has the side benefit of keeping our code fairly framework-agnostic)
inTransaction :: IsCtx ctx => ctx -> (Connection -> IO Rsp) -> Snap ()
inTransaction ctx actionThatReturnsAnRsp = do
   inTransactionMode ctx Serializable ReadWrite actionThatReturnsAnRsp

-- | An endpoint that uses 'inTransaction_readOnly' will keep responding even
--   when the server is in read-only mode.
-- 
--   Note that you the programmer are asserting the DB queries are read-only.
--   There's nothing in this library or in postgresql-simple which statically
--   checks that to be true!
inTransaction_readOnly :: IsCtx ctx => ctx -> (Connection -> IO Rsp) -> Snap ()
inTransaction_readOnly ctx f =
   inTransactionMode ctx Serializable ReadOnly f

-- | YOU SHOULD ONLY USE THIS ONCE
-- 
--   This lets you do a write transaction during read-only mode (not a
--     read-only transaction! _Our_ server read-only mode)
-- 
--   The only reason we need this is so that an admin can take us out of
--    read-only mode
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

   let transactMode = TransactionMode isolationLevel' readWriteMode'
   rsp <- liftIO $ Pool.withResource (ctxConnectionPool ctx) $ \conn ->
      E.mask $ \restore -> do
         PSQL.beginMode transactMode conn
         r <- restore (actionThatReturnsAResponse conn)
            `E.onException` rollback_ conn
         (case rspWillCommit r of
            ShouldCommit -> commit conn
            ShouldRollback -> rollback_ conn
            ) `E.onException` rollback_ conn -- To be safe. E.g. what if inspecting 'r' errors?
         pure r
   pureRsp ctx rsp

-- Sometimes you don't need a DB connection at all!
-- (I can't believe we haven't needed this function up until now!)
pureRsp :: IsCtx ctx => ctx -> Rsp -> Snap ()
pureRsp ctx = \case
-- TODO: is 'rollback' fine on readonly transactions? - look up
      -- Write success:
      RspGood v -> writeJSON $ ctx_wrapSuccess ctx v
      RspGoodLBS contentType x -> writeLBSSuccess_dontUseThis contentType x
      RspGoodRollback v -> writeJSON $ ctx_wrapSuccess ctx v

      -- Write error:
      RspBad err -> writeApiErr err
      RspBadCommit err -> writeApiErr err
      RspEmptyGood -> Snap.writeBS ""
      RspEmptyGoodRollback -> Snap.writeBS ""


-- Take a look at how postgresql-simple does it:
rollback_ :: Connection -> IO ()
rollback_ conn =
   rollback conn `E.catch` ((\_ -> return ()) :: IOError -> IO ())

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
