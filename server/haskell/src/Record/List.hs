module Record.List (list) where

import Auth.JWT (authorizedUserId)
import DB (execute)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Int (Int64)
import qualified Data.Text as T
import Data.Time (UTCTime)
import qualified Data.Vector as V
import qualified Hasql.Pool as HP
import qualified Hasql.Statement as S
import qualified Hasql.TH as TH
import Network.HTTP.Types (status200)
import Web.Scotty
  ( ScottyM,
    get,
    json,
    status,
  )

data ListResponse = GetResponse
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

$(deriveJSON defaultOptions ''ListResponse)

type RecordRow = (Int64, Int64, T.Text, T.Text, T.Text, T.Text, Int64, Double, T.Text, T.Text, UTCTime)

select :: S.Statement Int64 (V.Vector RecordRow)
select =
  [TH.vectorStatement|
    select
    id :: int8, board_size :: int8, name :: text, black_player :: text, white_player :: text, comment :: text, handicap :: int8, komi :: float8, ruleset :: text, winner :: text, created :: timestamptz
    from record_record
    where
    owner_id = $1 :: int8
    order by created desc
  |]

toResponse :: Int64 -> RecordRow -> ListResponse
toResponse userId (recordId, board_size, name, black_player, white_player, comment, handicap, komi, ruleset, winner, created) =
  GetResponse
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
  rows <- execute pool select userId
  status status200
  json $ V.map (toResponse userId) rows
