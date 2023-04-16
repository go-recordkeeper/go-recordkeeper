module Record.List (list) where

import Auth.JWT (authorizedUserId)
import Control.Monad (when)
import DB (execute)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Int (Int64)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Hasql.Pool as HP
import qualified Hasql.Statement as S
import qualified Hasql.TH as TH
import Network.HTTP.Types (status200, status404)
import Web.Scotty
  ( ScottyM,
    get,
    json,
    param,
    raiseStatus,
    rescue,
    status,
  )

data RecordResponse = RecordResponse
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

$(deriveJSON defaultOptions ''RecordResponse)

data ListResponse = ListResponse
  { count :: Int,
    pages :: Int,
    results :: Vector RecordResponse
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''ListResponse)

type RecordRow = (Int64, Int64, T.Text, T.Text, T.Text, T.Text, Int64, Double, T.Text, T.Text, UTCTime)

countStatement :: S.Statement Int64 Int64
countStatement =
  [TH.singletonStatement|
    select
    count(*) :: int8
    from record_record
    where
    owner_id = $1 :: int8
  |]

select :: S.Statement (Int64, Int64, Int64) (V.Vector RecordRow)
select =
  [TH.vectorStatement|
    select
    id :: int8, board_size :: int8, name :: text, black_player :: text, white_player :: text, comment :: text, handicap :: int8, komi :: float8, ruleset :: text, winner :: text, created :: timestamptz
    from record_record
    where
    owner_id = $1 :: int8
    order by created desc
    limit $2 :: int8
    offset $3 :: int8
  |]

toResponse :: Int64 -> RecordRow -> RecordResponse
toResponse userId (recordId, board_size, name, black_player, white_player, comment, handicap, komi, ruleset, winner, created) =
  RecordResponse
    { id = fromIntegral recordId,
      owner = fromIntegral userId,
      board_size = fromIntegral board_size,
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

list :: HP.Pool -> ScottyM ()
list pool = get "/api/records/" $ do
  userId <- authorizedUserId
  pageSize <- param "page_size" `rescue` (\_ -> return 10)
  page <- param "page" `rescue` (\_ -> return 1)
  when (pageSize < 1) $ raiseStatus status404 "Invalid page size"
  totalRecords <- execute pool countStatement userId
  let pages' = totalRecords `div` pageSize
      pages = if pages' == 0 then 1 else pages'
  when (page < 1 || page > pages) $ raiseStatus status404 "Invalid page number"
  rows <- execute pool select (userId, pageSize, (page - 1) * pageSize)
  status status200
  json $
    ListResponse
      { count = fromIntegral totalRecords,
        pages = fromIntegral pages,
        results = V.map (toResponse userId) rows
      }
