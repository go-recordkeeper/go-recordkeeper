module Record.Delete (deleteRecord) where

import Auth.JWT (authorizedUserId)
import DB (execute)
import Data.Int (Int64)
import qualified Hasql.Pool as HP
import qualified Hasql.Statement as S
import qualified Hasql.TH as TH
import Network.HTTP.Types (status204, status404)
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
  deleted <- execute pool deleteRecordStatement (userId, recordId)
  case deleted of
    0 -> do
      raiseStatus status404 "Does not exist."
    _ -> do
      -- 1 is expected
      -- More than 1 should be impossible due to DB constraints
      status status204
