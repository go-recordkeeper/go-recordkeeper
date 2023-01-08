{-# LANGUAGE OverloadedStrings #-}

module Lib (startApp) where

import Auth (authEndpoints)
import GHC.IO.Handle (hFlush)
import GHC.IO.Handle.FD (stdout)
import qualified Hasql.Connection as HC
import qualified Hasql.Pool as HP
import Network.Wai.Middleware.RequestLogger (logStdout)
import Record (recordEndpoints)
import Web.Scotty (middleware, scotty)

startApp :: IO ()
startApp = do
  putStrLn "Starting the server..."
  hFlush stdout
  -- let connectionSettings = HC.settings "localhost" 5432 "postgres" "postgres" "default"
  -- TODO test configuration only
  let connectionSettings = HC.settings "postgres" 5432 "postgres" "postgres" "default"
  pool <- HP.acquire (10, 30, connectionSettings)
  scotty 8000 $ do
    middleware logStdout
    authEndpoints pool
    recordEndpoints pool
