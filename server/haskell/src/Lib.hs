{-# LANGUAGE OverloadedStrings #-}

module Lib (startApp) where

import Auth
-- import Auth.User
import Crypto.JOSE (JWK)
import qualified Data.ByteString.Lazy.Char8 as BS
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

  scotty 8080 $ do
    middleware $ logStdout
    -- get "/foof" $ do
    --   -- word <- param "word"
    --   let word = "lol"
    --   html $ mconcat ["<h1> hello ", word, "</h1>"]
    -- get "/:word" $ do
    --   word <- param "word"
    --   html $ mconcat ["<h1> hello ", word, "</h1>"]
    authEndpoints