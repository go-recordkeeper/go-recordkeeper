module Record.Get (getRecord) where

import Auth.JWT (authorizedUserId)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Int (Int64)
import qualified Data.Text as T
import Data.Time (UTCTime)
import qualified Hasql.Pool as HP
import qualified Hasql.Session as HS
import qualified Hasql.Statement as S
import qualified Hasql.TH as TH
import Network.HTTP.Types (status200, status404, status500)
import Web.Scotty
  ( ScottyM,
    get,
    json,
    param,
    raiseStatus,
    status,
  )

data GetResponse = GetResponse
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
    winner :: String,
    -- TODO moves
    moves :: [Int],
    stones :: [Int]
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''GetResponse)

type RecordRow = (Int64, T.Text, T.Text, T.Text, T.Text, Int64, Double, T.Text, T.Text, UTCTime)

select :: S.Statement (Int64, Int64) RecordRow
select =
  [TH.singletonStatement|
    select
    board_size :: int8, name :: text, black_player :: text, white_player :: text, comment :: text, handicap :: int8, komi :: float8, ruleset :: text, winner :: text, created :: timestamptz
    from record_record
    where
    owner_id = $1 :: int8 and id = $2 :: int8
  |]

toResponse :: Int64 -> Int64 -> RecordRow -> GetResponse
toResponse userId recordId (board_size, name, black_player, white_player, comment, handicap, komi, ruleset, winner, created) =
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
      winner = T.unpack winner,
      -- TODO moves
      moves = [],
      stones = []
    }

getRecord :: HP.Pool -> ScottyM ()
getRecord pool = get "/api/records/:recordId/" $ do
  userId <- authorizedUserId
  recordId <- param "recordId"
  result <- liftIO $ HP.use pool $ HS.statement (userId, recordId) select
  case result of
    Right row -> do
      status status200
      json $ toResponse userId recordId row
    Left (HP.SessionError (HS.QueryError _ _ (HS.ResultError (HS.UnexpectedAmountOfRows 0)))) -> do
      raiseStatus status404 "Does not exist."
    Left _ -> do
      raiseStatus status500 "Error"
