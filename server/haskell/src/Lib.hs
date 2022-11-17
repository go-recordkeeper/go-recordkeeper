module Lib (startApp, app) where

import Auth (AuthAPI, authServer)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

type API = "api" :> AuthAPI

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = authServer
