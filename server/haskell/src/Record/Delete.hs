module Record.Delete (deleteRecord) where

import Auth.JWT (authorizedUserId)
import Control.Monad.IO.Class (liftIO)
import Data.Int (Int64)
import qualified Hasql.Pool as HP
import qualified Hasql.Session as HS
import qualified Hasql.Statement as S
import qualified Hasql.TH as TH
import Network.HTTP.Types (status204, status404, status500)
import Web.Scotty
  ( ScottyM,
    delete,
    param,
    raiseStatus,
    status,
  )

deleteRecordStatement :: S.Statement (Int64, Int64) Int64
deleteRecordStatement =
  [TH.rowsAffectedStatement|
    delete from record_record where
    owner_id = $1 :: int8 and id = $2 :: int8 
    |]

deleteRecord :: HP.Pool -> ScottyM ()
deleteRecord pool = delete "/api/records/:recordId/" $ do
  userId <- authorizedUserId
  recordId <- param "recordId"
  deleted <- liftIO $ HP.use pool $ HS.statement (userId, recordId) deleteRecordStatement
  case deleted of
    Right 1 -> do
      status status204
    Right 0 -> do
      raiseStatus status404 "Does not exist."
    _ -> do
      raiseStatus status500 "DB error"
