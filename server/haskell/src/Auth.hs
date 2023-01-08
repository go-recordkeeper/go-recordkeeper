module Auth (authEndpoints) where

import Auth.Get
import Auth.Login
import Auth.Register
import qualified Hasql.Pool as HP
import Web.Scotty

authEndpoints :: HP.Pool -> ScottyM ()
authEndpoints pool = do
  getUser pool
  login pool
  register pool
