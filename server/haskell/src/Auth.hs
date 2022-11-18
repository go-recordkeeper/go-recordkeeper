module Auth (AuthAPI, authServer) where

import Auth.Get (GetAPI, get)
import Auth.Login (LoginAPI, login)
import Auth.Register (RegisterAPI, register)
import Servant

type AuthAPI = LoginAPI :<|> RegisterAPI :<|> GetAPI

authServer :: Server AuthAPI
authServer = login :<|> register :<|> get
