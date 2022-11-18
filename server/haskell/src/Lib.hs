module Lib (startApp, app) where

import Auth (AuthAPI, authServer)
import Auth.User
import Crypto.JOSE (JWK)
import qualified Data.ByteString.Lazy.Char8 as BS
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Auth.Server

type API = "api" :> AuthAPI

api :: Proxy API
api = Proxy

server :: Server API
server = authServer

app :: JWK -> Application
app jwk = serveWithContext api (defaultCookieSettings :. defaultJWTSettings jwk :. EmptyContext) server

startApp :: IO ()
startApp = do
  jwk <- generateKey
  -- log a valid token for debug purposes
  token <- makeJWT (Auth.User.User 0 "Daniel" "daniel@chiquito" "password") (defaultJWTSettings jwk) Nothing
  case token of
    Left _ -> putStrLn "there was a jwt err"
    Right token' -> BS.putStrLn token'
  run 8080 (app jwk)
