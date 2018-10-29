{-# LANGUAGE
     BangPatterns
   , DeriveGeneric
   , ExistentialQuantification
   , FlexibleContexts
   , InstanceSigs
   , LambdaCase
   , OverloadedStrings
   , TypeFamilies
   , ViewPatterns
   #-}

module Gingersnap.Core (
   -- * Rsp
     Rsp(..)

   , rspGood
   , rspBad
   , rspBadCommit
   , rspBadRollback
   , rspGoodCSV
   , rspGoodLBS
   , rspEmptyGood

   -- * pureRsp
   , pureRsp

   -- * DB Transactions
   , inTransaction
   , inTransactionMode
   , inTransaction_readOnly
   , inTransaction_override
   , rspIsGood

   -- * IsCtx
   , IsCtx(..)

   -- * JSON requests
   , reqObject
   , reqObject'
   , (.!)
   , (.!?)
   , ReqObject(..)

   -- * Errors
   , errorEarlyCode

   , ApiErr(..)
   , ErrResult(..)
   , DefaultApiErr(..)

   , ctxErr

   -- * Internals
   -- These won't typically be inspected by hand but there's no reason we should
   --   block people from inspecting them if they like
   , RspPayload(..)
   , ShouldCommitOrRollback(..)

   -- * Reexports, for convenience
   , Pool
   , createPool
   , Connection

   -- Maybe?:
   -- , module import Network.HTTP.Types.Status
   -- , module Snap.Core
   ) where

import Control.DeepSeq -- (NFData(rnf), deepseq)
import qualified Control.Exception as E
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, fromJSON, ToJSON(..), (.=))
import qualified Data.Aeson as JSON
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import Data.Pool (Pool, createPool)
import qualified Data.Pool as Pool
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word
import Database.PostgreSQL.Simple (Connection)
import qualified Database.PostgreSQL.Simple as PSQL
import qualified Database.PostgreSQL.Simple.Transaction as PSQL
import GHC.Generics (Generic)
import qualified Network.HTTP.Types.Status as HTTP
import Snap.Core (Snap)
import qualified Snap.Core as Snap

-- | Don't be daunted! The only thing you need to provide (i.e. that doesn't
--     have a default value) is 'ctxConnectionPool'
class ApiErr (CtxErrType ctx) => IsCtx ctx where
   ctxConnectionPool :: ctx -> Pool Connection

   ctxGetReadOnlyMode :: ctx -> IO Bool
   ctxGetReadOnlyMode _ = pure False

   ctx_wrapSuccess :: ToJSON x => ctx -> x -> JSON.Value
   ctx_wrapSuccess _ x = JSON.object ["result" .= x]

   type CtxErrType ctx
   type instance CtxErrType ctx = DefaultApiErr

-- This just forces the error type like a Proxy
ctxErr :: IsCtx ctx => ctx -> (CtxErrType ctx) -> (CtxErrType ctx)
ctxErr _ x = x

class ApiErr apiErr where
   errResult :: apiErr -> ErrResult

   -- | The request object is missing a required key.
   --   E.g. the request is @{"first": "Tom"}@ but we need both a @"first"@ and a
   --   @"last"@
   apiErr_missingRequestKey :: Text -> apiErr

   -- | We can't process the request because the request is malformed JSON or
   --   not JSON at all
   apiErr_requestNotJSON :: apiErr

   -- | The request *is* JSON, but not an object (e.g. maybe it's an array
   --   or a number, but we need an object)
   apiErr_requestNotJSONObject :: apiErr

   -- | It's a JSON object but it's malformed somehow (e.g. maybe it's got the
   --   wrong keys). In other words, we can't 'fromJSON' it successfully.
   --
   --   (The 'Text' is the key of the malformed value)
   apiErr_malformedRequestValue :: Text -> JSON.Value -> apiErr

   -- | A 500 internal server error
   -- 
   --   The Text value is the error message. You may want different behavior in
   --     development vs. production, e.g. not showing internal errors in prod
   apiErr_unexpectedError :: Text -> apiErr

   apiErr_inReadOnlyMode :: apiErr

data ErrResult
   = ErrResult HTTP.Status JSON.Value
 deriving (Show, Eq) -- , Generic)

instance NFData ErrResult where
   -- ~~[Forcing HTTP.Status]~~
   -- There's no NFData instance for HTTP.Status, and I don't want to make an
   --   orphan instance for one, so we take it apart by hand instead:
   rnf (ErrResult (HTTP.Status code msg) v) =
      rnf code `seq` rnf msg `seq` rnf v

{-
deriving instance Generic HTTP.Status
instance NFData HTTP.Status
-}

-- Might be nice, since we don't have a Read.
-- But probably better for a 'pretty*' function
{-
instance Show ErrResult where
   show (ErrResult status j) =
      "ErrResult "++show status++" "++show (JSON.encode j)
-}

data DefaultApiErr
   = DefaultApiErr_ReadOnlyMode
   | DefaultApiErr_MissingRequestKey Text
   | DefaultApiErr_RequestNotJSON
   | DefaultApiErr_RequestNotJSONObject
   | DefaultApiErr_MalformedRequestValue Text JSON.Value
   | DefaultApiErr_UnexpectedError Text
   | DefaultApiErr_Custom HTTP.Status String [(Text, JSON.Value)]
 deriving (Show, Eq)

instance ApiErr DefaultApiErr where
   apiErr_inReadOnlyMode =
      DefaultApiErr_ReadOnlyMode
   apiErr_missingRequestKey =
      DefaultApiErr_MissingRequestKey
   apiErr_requestNotJSON =
      DefaultApiErr_RequestNotJSON
   apiErr_requestNotJSONObject =
      DefaultApiErr_RequestNotJSONObject
   apiErr_malformedRequestValue =
      DefaultApiErr_MalformedRequestValue
   apiErr_unexpectedError =
      DefaultApiErr_UnexpectedError

   errResult :: DefaultApiErr -> ErrResult
   errResult = defaultApiErrResult

defaultApiErrResult :: DefaultApiErr -> ErrResult
defaultApiErrResult err =
   ErrResult status  $
      JSON.object [
           "errorCode" .= (code :: Int)
         , "errorMessage" .= msg
         , "errorVals" .= (vals :: [(Text, JSON.Value)])
         ]
 where
    (code, status, msg, vals) = case err of
       DefaultApiErr_ReadOnlyMode ->
          (0, HTTP.serviceUnavailable503, "This action is unavailable in read-only mode", [])
       DefaultApiErr_UnexpectedError t ->
          (1, HTTP.internalServerError500, "An unexpected error occurred", [
               "text" .= t
             ])
       DefaultApiErr_MissingRequestKey k ->
          (2, HTTP.unprocessableEntity422, "Required key not present: "++show k, [
               "key" .= k
             ])
       DefaultApiErr_RequestNotJSON ->
          (3, HTTP.unprocessableEntity422, "Non-JSON message body", [])

       DefaultApiErr_RequestNotJSONObject ->
          (4, HTTP.unprocessableEntity422, "Message body is not a JSON object", [])

       DefaultApiErr_MalformedRequestValue k v ->
          (5, HTTP.unprocessableEntity422, "Malformed value: "++show k, [
               "key" .= k, "value" .= v
             ])
       DefaultApiErr_Custom s t vs ->
          (6, s, t, vs)

-- | How we construct responses. You probably don't want to be constructing or
--   inspecting them by hand; instead you can use 'rspGood', 'rspBadRollback', etc.
data Rsp
   = Rsp {
     rspShouldCommit :: ShouldCommitOrRollback
   , rspPayload :: RspPayload
   }
 deriving (Generic)

instance NFData Rsp

data RspPayload
   = forall x. ToJSON x => RspPayload_Good x
   | forall e. ApiErr e => RspPayload_Bad e
   -- | First ByteString is MIME type; second is response body
   | RspPayload_Custom HTTP.Status BS.ByteString BSL.ByteString
   | RspPayload_Empty -- This might be a dupe with '_Custom' but it's nice to have

instance NFData RspPayload where
   rnf = \case
      RspPayload_Empty -> ()
      -- RspPayload_Custom a b c -> (RspPayload_Custom $!! a <$!!> b <$!!> c) `seq` ()
      -- TODO: i don't need this '_ $!!' rite?:
      RspPayload_Good x -> (RspPayload_Good $!! (force $ toJSON x)) `seq` ()
      RspPayload_Bad e -> rnf $ errResult e
      RspPayload_Custom (HTTP.Status a b) c d ->
         -- See the note above about ~~[Forcing HTTP.Status]~~ :
         rnf a `seq` rnf b `seq` rnf c `seq` rnf d

data ShouldCommitOrRollback
   = ShouldCommit
   | ShouldRollback
 deriving (Show, Eq, Generic)

instance NFData ShouldCommitOrRollback

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
   RspPayload_Custom httpStatus _ _ ->
      httpStatus == HTTP.ok200
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

-- | Creates a read-only transaction and will keep responding even if the
--   server's in read-only mode.
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

-- | The most general version of 'inTransaction'.
-- 
--   An endpoint that uses 'ReadOnly' will keep responding even when the server
--   is in read-only mode.
inTransactionMode :: IsCtx ctx => ctx -> PSQL.IsolationLevel -> PSQL.ReadWriteMode -> (Connection -> IO Rsp) -> Snap ()
inTransactionMode ctx isolationLevel' readWriteMode' actionThatReturnsAResponse = do
   readOnlyMode <- liftIO $ ctxGetReadOnlyMode ctx
   when (readOnlyMode && (readWriteMode' /= PSQL.ReadOnly)) $
      errorEarlyCode $ ctxErr ctx apiErr_inReadOnlyMode

   inTransaction_internal ctx isolationLevel' readWriteMode' actionThatReturnsAResponse

-- | DON'T USE THIS FUNCTION! This should only be called by
--   'inTransaction_override' and 'inTransactionMode'
inTransaction_internal :: IsCtx ctx => ctx -> PSQL.IsolationLevel -> PSQL.ReadWriteMode -> (Connection -> IO Rsp) -> Snap ()
inTransaction_internal ctx isolationLevel' readWriteMode' actionThatReturnsAResponse = do

   let transactMode = PSQL.TransactionMode isolationLevel' readWriteMode'
   rsp <- liftIO $
      (Pool.withResource (ctxConnectionPool ctx) $ \conn ->
         E.mask $ \restore -> do
            PSQL.beginMode transactMode conn
            !r <- restore (force <$> actionThatReturnsAResponse conn)
               `E.onException` rollback_ conn
            (case rspShouldCommit r of
               ShouldCommit -> PSQL.commit conn
               -- Note it is safe to call rollback on a read-only transaction:
               -- https://www.postgresql.org/message-id/26036.1114469591%40sss.pgh.pa.us
               ShouldRollback -> rollback_ conn
               ) `E.onException` rollback_ conn -- To be safe. E.g. what if inspecting 'r' errors?
            pure r)
               `E.catch` (\e@(E.SomeException {}) -> pure $ rspBad $
                  ctxErr ctx $ apiErr_unexpectedError $ T.pack $ show e)
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
   PSQL.rollback conn
      `E.catch` ((\_ -> return ()) :: IOError -> IO ())

writeLBSSuccess_dontUseThis :: BS.ByteString -> BSL.ByteString -> Snap ()
writeLBSSuccess_dontUseThis contentType b = do
   Snap.modifyResponse $
      Snap.setHeader "Content-Type" contentType
   Snap.writeLBS b

writeJSON :: ToJSON x => x -> Snap ()
writeJSON x = do
   Snap.modifyResponse $
      Snap.setHeader "Content-Type" "application/json"
   Snap.writeLBS $ JSON.encode $ x

-- | This returns any 'Snap x' so you can use it like a throw anywhere
--     in your snap code
-- 
--   NOTE: if you ever use 's 'withTransaction' (which we don't recommend!)
--     this function has the same caveats as 'finishWith'

errorEarlyCode :: ApiErr ae => ae -> Snap x
errorEarlyCode err = do
   writeApiErr err
   Snap.getResponse >>= Snap.finishWith
-- Difference with 'pureRsp . rspBad' is that it actually 'finishWith's

-- TODO: alias to 'errorCode'?:
writeApiErr :: ApiErr ae => ae -> Snap ()
writeApiErr (errResult -> (ErrResult httpStatus responseVal)) = do
   Snap.modifyResponse $ Snap.setResponseCode $
      HTTP.statusCode httpStatus
   writeJSON $ toJSON responseVal

-- | Like (.!?) but returns a 422 error (with 'errorEarly') if the key isn't
--   present
(.!) :: (IsCtx ctx, FromJSON x) => ReqObject ctx -> Text -> Snap x
ro@(ReqObject ctx _) .! k = (ro .!? k) >>= \case
   Nothing -> errorEarlyCode $ ctxErr ctx $ apiErr_missingRequestKey k
   Just x -> pure x

-- | Get a JSON value from the request object, and give a HTTP 422
--   response ('errorEarly') if the value is malformed (not able to be decoded).
--   If it's not present, don't fail: just give us a Nothing
(.!?) :: (IsCtx ctx, FromJSON x) => ReqObject ctx -> Text -> Snap (Maybe x)
(ReqObject ctx hm) .!? k = case HM.lookup k hm of
   Nothing -> pure Nothing
   Just v -> case fromJSON v of
      JSON.Success x -> pure (Just x)
      _ -> errorEarlyCode $ ctxErr ctx $ apiErr_malformedRequestValue k v

data ReqObject ctx
   = ReqObject ctx (HM.HashMap Text JSON.Value)

reqObject :: IsCtx ctx => ctx -> Snap (ReqObject ctx)
reqObject ctx = reqObject' ctx 2048

reqObject' :: IsCtx ctx => ctx -> Word64 -> Snap (ReqObject ctx)
reqObject' ctx size =
   (JSON.decode <$> Snap.readRequestBody size) >>= \case
      Nothing -> errorEarlyCode $ ctxErr ctx apiErr_requestNotJSON
      Just (JSON.Object o) -> pure $ ReqObject ctx o
      Just _ -> errorEarlyCode $ ctxErr ctx apiErr_requestNotJSONObject
