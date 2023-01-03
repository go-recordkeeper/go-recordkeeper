module Auth (authEndpoints) where

import Auth.Get
import Auth.Login
import Auth.Register
import qualified Hasql.Pool as HP
import Web.Scotty

-- authEndpoints :: Server AuthAPI
authEndpoints :: HP.Pool -> ScottyM ()
authEndpoints pool = do
  getUser pool
  login
  register pool
