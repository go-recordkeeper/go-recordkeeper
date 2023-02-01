module Lib (startApp) where

import Auth (authEndpoints)
import qualified Data.ByteString.UTF8 as BSU
import GHC.IO.Handle (hFlush)
import GHC.IO.Handle.FD (stdout)
import qualified Hasql.Connection as HC
import qualified Hasql.Pool as HP
import Network.Wai.Middleware.RequestLogger (logStdout)
import Record (recordEndpoints)
import System.Environment (getEnv)
import Web.Scotty (middleware, scotty)

startApp :: IO ()
startApp = do
  putStrLn "Starting the server..."
  hFlush stdout
  dbName <- BSU.fromString <$> getEnv "POSTGRES_NAME"
  dbUser <- BSU.fromString <$> getEnv "POSTGRES_USER"
  dbPassword <- BSU.fromString <$> getEnv "POSTGRES_PASSWORD"
  dbHost <- BSU.fromString <$> getEnv "POSTGRES_HOST"
  let connectionSettings = HC.settings dbHost 5432 dbUser dbPassword dbName
  pool <- HP.acquire (10, 30, connectionSettings)
  scotty 8000 $ do
    middleware logStdout
    authEndpoints pool
    recordEndpoints pool
