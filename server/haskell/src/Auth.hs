module Auth (authEndpoints) where

import Auth.Get
import Auth.Login
import Auth.Register
import Web.Scotty

-- authEndpoints :: Server AuthAPI
authEndpoints :: ScottyM ()
authEndpoints = do
  getUser
  login
  register
