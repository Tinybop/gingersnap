# Gingersnap

Gingersnap's not a web framework: that's `snap-core`'s job.
More a set of lightweight idioms for building a resource-safe JSON API with
`postgresql-simple`.

As it's just a set of idioms, it's easy to only use 'em where you need 'em.
An app could have only a single endpoint that uses Gingersnap, with the rest
using plain `snap` or `snap-core`.

How do we use it? This README is also a Literate Haskell file so it's a full
example you can run with markdown-unlit. Let's get started:

## Imports at the top!

A few imports we'll need for this tutorial:

```haskell
{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

import Data.Aeson (ToJSON, (.=))
import Database.PostgreSQL.Simple
 -- For our automatic JSON instance:
import GHC.Generics (Generic)
import Gingersnap.Core
import Network.HTTP.Types.Status (internalServerError500)
import Snap.Core
-- From the 'snap-server' package:
import Snap.Http.Server (quickHttpServe)
```

## A first endpoint

Now that we've got our imports, let's jump into defining an endpoint. We'll
define a little bit of setup code later on in the file.

```haskell
data Character = Character { name :: String, age :: Int }
 deriving (Show, Generic)

instance ToJSON Character

one :: Ctx -> Snap ()
one ctx =
   pureRsp ctx $ rspGood $ Character { name = "Yoda", age = 900 }
```

You can run the code from this file with

    $ cabal update
    $ cabal install gingersnap snap-server
    $ ghci -pgmL markdown-unlit README.lhs   # The "-pgmL" is just for this README

And calling "main". In another window, if you call:

    $ curl 'localhost:8000/one'

You should get back:

    {"result":{"age":900,"name":"Yoda"}}

A few things to notice:
  - The endpoint takes as an argument a "Ctx". We'll see the definition of that
    later.
  - The endpoint returns our data with "rspGood". More on that in a moment.
  - The response came wrapped in a "result" JSON object. You can customize that
    behavior but we'll use the default here.

So, what's "rspGood"? Well, it has the type

    rspGood :: ToJSON x => x -> Rsp

The "Rsp" type is one of the core types in Gingersnap. Keep an eye out for it later.

## Defining "main"

Let's now look at how we defined our main function.

```haskell
main :: IO ()
main = do
   ctx <- makeCtx
   quickHttpServe $ route [
        ("one", one ctx)
      , ("two", two ctx)
      , ("three", three ctx)
      ]
```

Other than "ctx", this isn't Gingersnap-specific at all: just a simple
`snap-core` server. "makeCtx" is a function we define ourselves. It creates a
value of type "Ctx", which we define ourselves, and which is an instance of
"IsCtx".

## IsCtx

The idea of "IsCtx" is that it allows us to thread whatever data we need through
to our endpoints. We'll definitely need a database connection (pool), but it's a
typeclass, so you can define whatever other fields you'd like to pass to your
handlers in the type that's an instance of that class.

For example, if you're using the 'auto-update' package to efficiently run
periodic actions (like getting the current time), you may want to create another
set of fields in your "Ctx" type to easily thread auto-update's actions through
to your handlers, too.

So let's define our own! We unimaginitavely call it "Ctx":

```haskell
data Ctx = Ctx { ctx_db :: Pool Connection }

instance IsCtx Ctx where
   ctxConnectionPool = ctx_db
```

And then define a simple "makeCtx":

```haskell
makeCtx :: IO Ctx
makeCtx = do

   -- Setting up the DB connection pool:
   let connString = " host=localhost port=5432 dbname=postgres user=postgres "
   pool <- createPool (connectPostgreSQL connString) close 1 5 20

   pure $ Ctx { ctx_db = pool }
```

And that's that!

## Talking to the database

Now that we've got (through Ctx) a DB connection pool, let's query the DB:


```haskell
two :: Ctx -> Snap ()
two ctx = do
   inTransaction ctx $ \conn -> do
      [Only x] <- query_ conn " SELECT 3 + 3 "
      pure $ rspGood $ Character { name = "Calvin", age = x }
```

    $ curl 'localhost:8000/two'
    {"result":{"age":6,"name":"Calvin"}} ~ $

Nice! This uses "inTransaction", another core tool in Gingersnap:

    inTransaction :: IsCtx ctx => ctx -> (Connection -> IO Rsp) -> Snap ()

We've already seen IsCtx and Rsp, so the main thing to notice here is that the
action we pass to inTransaction is passed a Connection and is in IO, not in Snap
(or MonadSnap). This gives us a few nice things:
  - If you're in Snap, you can accidentally leak a Connection resource by calling
    finishWith while in the middle of a transaction or DB action. We don't have this
    problem.
  - We can choose at any time whether to commit or rollback. The "Rsp" type carries
    information about whether to commit or rollback (e.g. "rspGood" will commit,
    "rspBadRollback" won't). This is in contrast to "withTransaction", that'll
    only roll back if there's an exception, and in contrast to manually beginning
    a transaction, which provides no check that we properly commit or rollback
    (e.g. we could forget to commit/rollback in one case among many in a complicated
    branching expression)

## Not everything's rspGood

What if we don't want to send a successful ok200 message? Or maybe we don't even
want to commit our transaction? These might come in handy then:

    rspBadCommit :: ApiErr ae => ae -> Rsp
    rspBadRollback :: ApiErr ae => ae -> Rsp

(`rspBad` is an alias for `rspBadRollback`)

`ApiErr` is a simple typeclass that ensures our error type can be converted to
a HTTP status code and JSON value.

We don't need to (and won't be) exploring the `ApiErr` typeclass here directly
, since we've got a good default instance with the `DefaultApiErr` type.
It'll make us write a little more (e.g. explicitly stating the status code of
our error), but it's quick and easy.

```haskell
three :: Ctx -> Snap ()
three ctx =
   inTransaction ctx $ \conn -> do
      [Only n] <- query_ conn " SELECT random () "
      pure $ if n >= (0.5 :: Double)
         then rspGood n
         else rspBad $
            DefaultApiErr_Custom
               internalServerError500
               "'n' too small"
               [("n" .= n)]
```

    $ curl -v 'localhost:8000/three'
    [...]
    < HTTP/1.1 200 OK
    [...]
    {"result":0.594665706157684}

    $ curl -v 'localhost:8000/three'
    [...]
    < HTTP/1.1 500 Internal Server Error
    [...]
    {"errorCode":6,"errorVals":[["n",0.250084751285613]],"errorMessage":"'n' too small"}

If you'd like to write your own `ApiErr` instance (which you probably should if
you're building something "real":
  - The definition of "errResult" for "DefaultApiErr" might be a good guide
    for how to get a consistent JSON result.
  - You'll want to make that type the "CtxErrType" associated type for IsCtx

## Not everything's JSON

A JSON API is usually JSON but if you'd like to e.g. send a CSV or a file there
are functions like `rspGoodCSV` and (the most general) `rspGoodLBS`.

If you don't see a function you need it's easy to define your own.

## Other things to discover

This tutorial is a work in progress, and these'll be the next concepts to be
touched on:

  - reqObject and (.!) for receiving JSON
  - returning JSON even when throwing an error
