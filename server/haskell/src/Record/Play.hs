module Record.Play (play) where

import Auth.JWT (authorizedUserId)
import DB (execute)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Int (Int64)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Hasql.Pool as HP
import qualified Hasql.Statement as S
import qualified Hasql.TH as TH
import Network.HTTP.Types (status201, status403)
import Record.Go (Color (Black, White), GoError (OutOfBounds, SpaceOccupied, Suicide), Move, playStones, runBoardA, toCoord', toPos')
import Web.Scotty
  ( ActionM,
    ScottyM,
    json,
    jsonData,
    param,
    post,
    raiseStatus,
    status,
  )

data PlayRequest = PlayRequest {x :: Int, y :: Int} deriving (Eq, Show)

$(deriveJSON defaultOptions ''PlayRequest)

data Stone = Stone {color :: T.Text, x :: Int, y :: Int} deriving (Eq, Show)

$(deriveJSON defaultOptions ''Stone)

data Point = Point {x :: Int, y :: Int} deriving (Eq, Show)

$(deriveJSON defaultOptions ''Point)

data PlayResponse = PlayResponse {add :: [Stone], remove :: [Point]} deriving (Eq, Show)

$(deriveJSON defaultOptions ''PlayResponse)

type RecordRow = (Int64, Int64)

selectRecord :: S.Statement (Int64, Int64) RecordRow
selectRecord =
  [TH.singletonStatement|
    select
    board_size :: int8, handicap :: int8
    from record_record
    where
    owner_id = $1 :: int8 and id = $2 :: int8
  |]

type MoveRow = (Maybe Int64, T.Text)

selectMoves :: S.Statement Int64 (V.Vector MoveRow)
selectMoves =
  [TH.vectorStatement|
    select
    position :: int8?, color :: text
    from record_move
    where
    record_id = $1 :: int8
  |]

type InsertMoveRow = (Int64, Int64, T.Text, Int64)

insertMove :: S.Statement InsertMoveRow Int64
insertMove =
  [TH.rowsAffectedStatement|
    insert into record_move
    (record_id, position, color, move)
    values
    ($1 :: int8, $2 :: int8, $3 :: text, $4 :: int8)
    |]

toColor :: T.Text -> Color
toColor color' = case color' of
  "B" -> Black
  "W" -> White
  _ -> White -- TODO????

fromColor :: Color -> T.Text
fromColor color = case color of
  Black -> "B"
  White -> "W"

nextColor :: Int -> [Move] -> Color
nextColor handicap moves =
  if null moves || length moves < handicap || snd (last moves) == White
    then Black
    else White

play :: HP.Pool -> ScottyM ()
play pool = post "/api/records/:recordId/play/" $ do
  userId <- authorizedUserId
  recordId <- param "recordId"
  PlayRequest {x, y} <- jsonData :: ActionM PlayRequest
  (size', handicap') <- execute pool selectRecord (userId, recordId)
  moves' <- execute pool selectMoves recordId
  let size = fromIntegral size'
      handicap = fromIntegral handicap'
      moves = [(fmap fromIntegral position', toColor color') | (position', color') <- V.toList moves']
      position = toPos' size (x, y)
      moveColor = nextColor handicap moves
  case runBoardA size $ playStones $ moves ++ [(Just position, moveColor)] of
    Right (_, captures) -> do
      let removals = [Point {x = x', y = y'} | (x', y') <- map (toCoord' size) $ Set.toAscList captures]
      _ <- execute pool insertMove (recordId, fromIntegral position, fromColor moveColor, fromIntegral $ 1 + length moves)
      status status201
      json PlayResponse {add = [Stone {color = fromColor moveColor, x, y}], remove = removals}
    Left (Suicide _) -> raiseStatus status403 "Move is suicidal"
    Left (SpaceOccupied _) -> raiseStatus status403 "Already a stone there"
    Left (OutOfBounds _) -> raiseStatus status403 "Out of bounds"
