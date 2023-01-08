module Record (recordEndpoints) where

import qualified Hasql.Pool as HP
import Record.Create (create)
import Record.Get (getRecord)
import Web.Scotty (ScottyM)

recordEndpoints :: HP.Pool -> ScottyM ()
recordEndpoints pool = do
  create pool
  getRecord pool
