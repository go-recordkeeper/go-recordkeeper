module DB (execute) where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isJust)
import qualified Data.Text.Lazy as TL
import qualified Hasql.Pool as HP
import qualified Hasql.Session as HS
import qualified Hasql.Statement as S
import Network.HTTP.Types (status404, status500)
import System.Environment (lookupEnv)
import Web.Scotty (ActionM, raiseStatus)

isDebug :: IO Bool
isDebug = do
  env <- liftIO $ lookupEnv "GOBAN_DEVELOPMENT"
  return $ isJust env

execute :: HP.Pool -> S.Statement a b -> a -> ActionM b
execute pool query args = do
  debug <- liftIO isDebug
  result <- liftIO $ HP.use pool $ HS.statement args query
  case result of
    Right value -> return value
    Left (HP.SessionError (HS.QueryError _ _ (HS.ResultError (HS.UnexpectedAmountOfRows 0)))) -> do
      raiseStatus status404 "Does not exist."
    Left err -> do
      if debug
        then raiseStatus status500 $ TL.pack $ show err
        else raiseStatus status500 "DB error."
