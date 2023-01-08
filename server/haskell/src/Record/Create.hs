module Record.Create (create) where

import Auth.JWT (authorizedUserId)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time (UTCTime)
import qualified Data.Time as Clock
import qualified Hasql.Pool as HP
import qualified Hasql.Session as HS
import qualified Hasql.Statement as S
import qualified Hasql.TH as TH
import Network.HTTP.Types (status201, status400, status500)
import Web.Scotty
  ( ActionM,
    ScottyM,
    json,
    jsonData,
    liftAndCatchIO,
    post,
    raiseStatus,
    status,
  )

data CreateRequest = CreateRequest
  { board_size :: Int,
    name :: Maybe String,
    black_player :: Maybe String,
    white_player :: Maybe String,
    comment :: Maybe String,
    handicap :: Maybe Int,
    komi :: Maybe Double,
    ruleset :: Maybe String
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''CreateRequest)

data CreateResponse = CreateResponse
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

$(deriveJSON defaultOptions ''CreateResponse)

type RecordRow = (Int64, Int64, T.Text, T.Text, T.Text, T.Text, Int64, Double, T.Text, T.Text, UTCTime)

insert :: S.Statement RecordRow Int64
insert =
  [TH.singletonStatement|
    insert into record_record
    (owner_id, board_size, name, black_player, white_player, comment, handicap, komi, ruleset, winner, created)
    values
    ($1 :: int8, $2 :: int8, $3 :: text, $4 :: text, $5 :: text, $6 :: text, $7 :: int8, $8 :: float8, $9 :: text, $10 :: text, $11 :: timestamptz)
    returning id :: int8
  |]

withDefaults :: Int64 -> UTCTime -> CreateRequest -> RecordRow
withDefaults userId now request = do
  ( userId,
    fromIntegral board_size,
    T.pack $ fromMaybe "" name,
    T.pack $ fromMaybe "Black" black_player,
    T.pack $ fromMaybe "White" white_player,
    T.pack $ fromMaybe "" comment,
    fromIntegral $ fromMaybe 0 handicap,
    fromMaybe 7.5 komi,
    T.pack $ fromMaybe "AGA" ruleset,
    "U",
    now
    )
  where
    CreateRequest {board_size, name, black_player, white_player, comment, handicap, komi, ruleset} = request

toResponse :: Int64 -> RecordRow -> CreateResponse
toResponse recordId (userId, board_size, name, black_player, white_player, comment, handicap, komi, ruleset, winner, created) =
  CreateResponse
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

create :: HP.Pool -> ScottyM ()
create pool = post "/api/records/" $ do
  userId <- authorizedUserId
  request <- jsonData :: ActionM CreateRequest
  let CreateRequest {board_size} = request
  when (board_size /= 9 && board_size /= 13 && board_size /= 19) $ raiseStatus status400 "Invalid board size"
  now <- liftAndCatchIO Clock.getCurrentTime
  let row = withDefaults userId now request
  result <- liftIO $ HP.use pool $ HS.statement row insert
  case result of
    Right recordId -> do
      status status201
      json $ toResponse recordId row
    Left err -> do
      raiseStatus status500 $ TL.pack $ show err
