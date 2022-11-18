module Auth (AuthAPI, authServer) where

import Auth.Get
import Auth.Login
import Auth.Register
import Servant

type AuthAPI = LoginAPI :<|> RegisterAPI :<|> GetAPI

authServer :: Server AuthAPI
authServer = login :<|> register :<|> get
