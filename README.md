# Gingersnap

What _is_ Gingersnap? Not a web framework: that's Snap Core's job.
More a set of lightweight idioms for building a resource-safe JSON API with
Aeson and postgresql-simple.

As it's just a set of idioms, it's easy to only use 'em where you need 'em.
An app could have only a single endpoint that uses Gingersnap, with the rest
using plain Snap Core.

How do we use it? This README is also a Literate Haskell file so it's a full
example you can run with markdown-unlit. Let's get started:

## Imports at the top!

A few imports we'll need for this tutorial:

```haskell
{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

import Data.Aeson (ToJSON)
import qualified Database.PostgreSQL.Simple as PSQL
 -- For our automatic JSON instance:
import GHC.Generics (Generic)
import Gingersnap.Core
import Snap.Core
-- From the 'snap-server' package:
import Snap.Http.Server (quickHttpServe)
```

## A first endpoint

Now that we've got our imports, let's jump into defining an endpoint. We'll
define a little bit of setup code later on in the file.

```haskell
data SomeData = SomeData Int Bool
 deriving (Show, Generic)

instance ToJSON SomeData

one :: Ctx -> Snap ()
one ctx =
   pureRsp ctx $ rspGood $ SomeData 5 True
```

You can run the code from this file with

    $ cabal update
    $ cabal install gingersnap snap-server
    $ ghci -pgmL markdown-unlit README.lhs   # The "-pgmL" is just for this README

And calling "main". In another window, if you call:

    $ curl 'localhost:8000/one'

You should get back:

    {"result":[5,true]}

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
      ]
```

Other than "ctx", this isn't Gingersnap-specific at all: just a simple
Snap Core server. "makeCtx" is a function we define ourselves. It creates a
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
data Ctx = Ctx { ctx_db :: Pool PSQL.Connection }

instance IsCtx Ctx where
   ctxConnectionPool = ctx_db
```

And then define a simple "makeCtx":

```haskell
makeCtx :: IO Ctx
makeCtx = do

   -- Setting up the DB connection pool:
   let connString = " host=localhost port=5432 dbname=postgres user=postgres "
   pool <- createPool (PSQL.connectPostgreSQL connString) PSQL.close 1 5 20

   pure $ Ctx { ctx_db = pool }
```

And that's that!

## Talking to the database

Now that we've got (through Ctx) a DB connection pool, let's query the DB:


```haskell
two :: Ctx -> Snap ()
two ctx = do
   inTransaction ctx $ \conn -> do
      [PSQL.Only x] <- PSQL.query_ conn " SELECT 2 + 2 "
      pure $ rspGood $ SomeData x True
```

    $ curl 'localhost:8000/two'
    {"result":[4,true]}

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
    statement)


## Other things to discover

This tutorial is a work in progress, and these'll be the next concepts to be
touched on:

  - "Not everything's rspGood": rspBadRollback, ApiErr, and others
  - reqObject and (.!) for consuming JSON
