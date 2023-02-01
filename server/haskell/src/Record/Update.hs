module Record.Update (update) where

import Auth.JWT (authorizedUserId)
import DB (execute)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Int (Int64)
import qualified Data.Text as T
import Data.Time (UTCTime)
import qualified Hasql.Pool as HP
import qualified Hasql.Statement as S
import qualified Hasql.TH as TH
import Network.HTTP.Types (status200)
import Web.Scotty
  ( ActionM,
    ScottyM,
    json,
    jsonData,
    param,
    put,
    status,
  )

data UpdateRequest = UpdateRequest
  { name :: String,
    black_player :: String,
    white_player :: String,
    comment :: String,
    handicap :: Int,
    komi :: Double,
    ruleset :: String,
    winner :: String
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''UpdateRequest)

data UpdateResponse = UpdateResponse
  { id :: Int,
    owner :: Int,
    board_size :: Int,
    created :: UTCTime,
    name :: String,
    black_player :: String,
    white_player :: String,
    comment :: String,
    handicap :: Int,
    komi :: Double,
    ruleset :: String,
    winner :: String
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''UpdateResponse)

type RecordRow = (Int64, Int64, T.Text, T.Text, T.Text, T.Text, Int64, Double, T.Text, T.Text)

updateRecord :: S.Statement RecordRow (Int64, UTCTime)
updateRecord =
  [TH.singletonStatement|
    update record_record set
      name = $3 :: text,
      black_player = $4 :: text,
      white_player = $5 :: text,
      comment = $6 :: text,
      handicap = $7 :: int8,
      komi = $8 :: float8,
      ruleset = $9 :: text,
      winner = $10 :: text
    where
    owner_id = $1 :: int8 and id = $2 :: int8
    returning
    board_size :: int8, created :: timestamptz
  |]

toRow :: Int64 -> Int64 -> UpdateRequest -> RecordRow
toRow userId recordId UpdateRequest {name, black_player, white_player, comment, handicap, komi, ruleset, winner} =
  ( userId,
    recordId,
    T.pack name,
    T.pack black_player,
    T.pack white_player,
    T.pack comment,
    fromIntegral handicap,
    komi,
    T.pack ruleset,
    T.pack winner
  )

toResponse :: RecordRow -> Int64 -> UTCTime -> UpdateResponse
toResponse (userId, recordId, name, black_player, white_player, comment, handicap, komi, ruleset, winner) size created =
  UpdateResponse
    { id = fromIntegral recordId,
      owner = fromIntegral userId,
      board_size = fromIntegral size,
      created,
      name = T.unpack name,
      black_player = T.unpack black_player,
      white_player = T.unpack white_player,
      comment = T.unpack comment,
      handicap = fromIntegral handicap,
      komi,
      ruleset = T.unpack ruleset,
      winner = T.unpack winner
    }

update :: HP.Pool -> ScottyM ()
update pool = put "/api/records/:recordId/" $ do
  userId <- authorizedUserId
  recordId <- param "recordId"
  request <- jsonData :: ActionM UpdateRequest
  let row = toRow userId recordId request
  (size, created) <- execute pool updateRecord row
  status status200
  json $ toResponse row size created
