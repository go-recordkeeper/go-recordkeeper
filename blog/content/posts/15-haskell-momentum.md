---
title: "Gaining Momentum with Haskell"
date: 2023-01-02T23:19:49-05:00
type: post
slug: "15-haskell-momentum"
tags:
  - haskell
---

I'm finally getting some traction with Haskell, even if the going is slow. I've "finished" the register endpoint to the point that it passes the integration test:

```haskell
module Auth.Register (register) where

-- 30 (!) lines of imports

data RegisterRequest = RegisterRequest
  { username :: String,
    email :: String,
    password :: String
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''RegisterRequest)

data RegisterResponse = RegisterResponse
  { id :: Int,
    username :: String,
    email :: String
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''RegisterResponse)

insert :: S.Statement (Text, Text, Text, UTCTime) Int64
insert =
  [TH.rowsAffectedStatement|
    insert into auth_user
    (username, email, password, date_joined, last_login, first_name, last_name, is_superuser, is_staff, is_active)
    values
    ($1 :: text, $2 :: text, $3 :: text, $4 :: timestamptz, $4 :: timestamptz, '', '', false, false, false)
  |]

register :: HP.Pool -> ScottyM ()
register pool = post "/api/register/" $ do
  RegisterRequest {username, email, password} <- jsonData :: ActionM RegisterRequest
  if not $ isValid $ BS.pack email
    then raiseStatus status400 "Invalid email"
    else do
      -- TODO hash the password
      now <- liftAndCatchIO Clock.getCurrentTime
      let sess = HS.statement (pack username, pack email, pack password, now) insert
      result <- liftIO $ HP.use pool sess
      case result of
        Right id' -> do
          status status201
          json (RegisterResponse {id = fromIntegral id', username, email})
        Left err ->
          case err of
            SessionError (QueryError _ _ (ResultError (HS.ServerError "23505" _ _ _))) -> raiseStatus status400 "A user with that username already exists."
            _ -> raiseStatus status500 $ Lazy.pack $ show err
```

The `register` function parses the request body, checks if the email is valid, attempts to insert the user into the database, and returns the newly created user as a JSON object. Appropriate errors are thrown if the email is invalid, the user already exists, or some arbitrary DB error occurred.

The observant reader will note that there is a `TODO` to hash the password before inserting it into the database. Lucky for me the integration test doesn't check that, so I'll take my 🗸.

I had already gotten the hasql connection pool set up, so all I had to do was figure out how to define the SQL statement to perform the insert. Template Haskell makes it pretty much just normal SQL, which is nice. I've heard horror stories about Template Haskell, so it is fortunate that none of my queries are very complicated.

The most obnoxious part was working with String type conversions.

* As I understand it, the `String` type is the same as a `[Char]`. Since lists in Haskell are linked, this makes for atrocious performance for long strings.
* To get around this, there is an alternative datatype called `Text` which is backed by actual arrays.
* There is also a very similar but completely distinct datatype called `Text.Lazy` which has the same API, but doesn't actually require that the entire string reside in memory at the same time.
* Finally, there is a datatype `ByteString`, which is pretty much the same as `Text` except it uses 8-bit bytes instead of 16-bit unicode code points.

All the above stringy things are used in the above code, and all conversions must be explicit. It's not that bad to look at since they're all just strings, but it's obnoxious trying to deduce which particular `pack`/`unpack` function needs to be imported and invoked to get everything glued together.

The other main pain point today was the compile time. The integration testing suite requires a docker container for all the server implementations, so I set up a neat little multistage build that builds all the dependencies first, then the source code. Testing a change involved ~7 seconds of waiting for the image to rebuild, which is not ideal. Adding a new dependency (which is a tragically frequent occurrence, the standard library doesn't cover much) required a full rebuild, which took ~3 minutes, which is just ridiculous to me. Sadly I think this is just how long stack builds take.

## Next steps
* The password hashing algorithm is stolen from Django and is [already implemented](https://github.com/go-recordkeeper/go-recordkeeper/blob/3e5ea5a87d614db52165936f333f7d3b3ad8673c/server/fastapi/goban_server_fastapi/auth/password.py) in FastAPI, so it's just a matter of porting/testing that code in Haskell.
* Now that new users can be added to the DB, it's possible to start on the login test, which would also require the password hashing algorithm, in addition to the JWT signing code.
* With the JWT signing/verification code, getting the current user is possible.
* With all the auth endpoints done, it will be time to refactor that chunk of code for maximum legibility, and to try out the Haskell unit testing experience.
