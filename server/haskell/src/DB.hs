module DB (execute, execute') where

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
isDebug = isJust <$> lookupEnv "GOBAN_DEVELOPMENT"

execute' :: (HP.UsageError -> ActionM b) -> HP.Pool -> S.Statement a b -> a -> ActionM b
execute' errorHandler pool query args = do
  result <- liftIO $ HP.use pool $ HS.statement args query
  case result of
    Right value -> return value
    Left err -> errorHandler err

defaultErrorHandler :: HP.UsageError -> ActionM b
defaultErrorHandler err = do
  debug <- liftIO isDebug
  case err of
    HP.SessionError (HS.QueryError _ _ (HS.ResultError (HS.UnexpectedAmountOfRows 0))) -> do
      raiseStatus status404 "Does not exist."
    _ -> do
      if debug
        then raiseStatus status500 $ TL.pack $ show err
        else raiseStatus status500 "DB error."

execute :: (Show a, Show b) => HP.Pool -> S.Statement a b -> a -> ActionM b
execute pool query args = do
  liftIO $ print args
  result <- execute' defaultErrorHandler pool query args
  liftIO $ print result
  return result
