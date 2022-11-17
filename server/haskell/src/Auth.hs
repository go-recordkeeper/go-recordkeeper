module Auth (AuthAPI, authServer) where

import Auth.Get (GetAPI, get)
import Auth.Login (LoginAPI, login)
import Auth.Register (RegisterAPI, register)
import Servant

type AuthAPI auths =
  -- LoginAPI :<|> RegisterAPI :<|> GetAPI auths
  GetAPI auths

authServer :: Server (AuthAPI auths)
-- authServer auths = login :<|> register :<|> get auths
authServer = get