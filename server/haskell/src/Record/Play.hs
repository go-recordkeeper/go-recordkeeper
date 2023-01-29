module Record.Play (play) where

import Auth.JWT (authorizedUserId)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Int (Int64)
import qualified Data.IntMap.Strict as IntMap
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Time (UTCTime)
import qualified Data.Vector as V
import qualified Hasql.Pool as HP
import qualified Hasql.Session as HS
import qualified Hasql.Statement as S
import qualified Hasql.TH as TH
import Network.HTTP.Types (status201, status400, status403, status404, status500)
import Record.Go (Color (Black, White), GoError (OutOfBounds, SpaceOccupied, Suicide), Move, identifyCaptures, playStones, runBoardA, toCoord', toPos')
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

type MoveRow = (Int64, T.Text)

selectMoves :: S.Statement Int64 (V.Vector MoveRow)
selectMoves =
  [TH.vectorStatement|
    select
    position :: int8, color :: text
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
  recordSelect <- liftIO $ HP.use pool $ HS.statement (userId, recordId) selectRecord
  movesSelect <- liftIO $ HP.use pool $ HS.statement recordId selectMoves
  case (recordSelect, movesSelect) of
    (Right (size', handicap'), Right moves') -> do
      let moves = [(fromIntegral position', toColor color') | (position', color') <- V.toList moves']
          size = fromIntegral size'
          handicap = fromIntegral handicap'
          position = toPos' size (x, y)
          moveColor = nextColor handicap moves
      case runBoardA size $ playStones $ moves ++ [(position, moveColor)] of
        Right (_, captures) -> do
          let removals = [Point {x = x', y = y'} | (x', y') <- map (toCoord' size) $ Set.toAscList captures]
          result <- liftIO $ HP.use pool $ HS.statement (fromIntegral recordId, fromIntegral position, fromColor moveColor, fromIntegral $ 1 + length moves) insertMove
          case result of
            Right _ -> do
              status status201
              json PlayResponse {add = [Stone {color = fromColor moveColor, x, y}], remove = removals}
            Left _ -> do
              raiseStatus status500 "DB error"
        Left (Suicide _) -> raiseStatus status403 "Move is suicidal"
        Left (SpaceOccupied _) -> raiseStatus status403 "Already a stone there"
        Left (OutOfBounds _) -> raiseStatus status403 "Out of bounds"
    (Left (HP.SessionError (HS.QueryError _ _ (HS.ResultError (HS.UnexpectedAmountOfRows 0)))), _) -> do
      raiseStatus status404 "Does not exist."
    (_, _) -> do
      raiseStatus status500 "DB error"
