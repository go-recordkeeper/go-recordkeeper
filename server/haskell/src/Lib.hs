{-# LANGUAGE OverloadedStrings #-}

module Lib (startApp) where

import Auth
-- import Auth.User
import Crypto.JOSE (JWK)
import qualified Data.ByteString.Lazy.Char8 as BS
import GHC.IO.Handle (hFlush)
import GHC.IO.Handle.FD (stdout)
import qualified Hasql.Connection as HC
import qualified Hasql.Pool as HP
import Network.Wai.Middleware.RequestLogger (logStdout)
import Web.Scotty

startApp :: IO ()
startApp = do
  -- jwk <- generateKey
  -- log a valid token for debug purposes
  -- token <- makeJWT (Auth.User.User 0 "Daniel" "daniel@chiquito" "password") (defaultJWTSettings jwk) Nothing
  -- case token of
  --   Left _ -> putStrLn "there was a jwt err"
  --   Right token' -> BS.putStrLn token'
  putStrLn "Starting the server..."
  hFlush stdout
  -- let connectionSettings = HC.settings "localhost" 5432 "postgres" "postgres" "default"
  -- TODO test configuration only
  let connectionSettings = HC.settings "postgres" 5432 "postgres" "postgres" "default"
  pool <- HP.acquire (10, 30, connectionSettings)
  scotty 8000 $ do
    middleware logStdout
    -- get "/foof" $ do
    --   -- word <- param "word"
    --   let word = "lol"
    --   html $ mconcat ["<h1> hello ", word, "</h1>"]
    -- get "/:word" $ do
    --   word <- param "word"
    --   html $ mconcat ["<h1> hello ", word, "</h1>"]
    authEndpoints pool
