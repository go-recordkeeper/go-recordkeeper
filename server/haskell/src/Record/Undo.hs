module Record.Undo (undo) where

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
import Network.HTTP.Types (status200, status403, status500)
import Record.Go (Color (Black, White), playStones, runBoardA, toCoord')
import Web.Scotty
  ( ScottyM,
    json,
    param,
    post,
    raiseStatus,
    status,
  )

data Stone = Stone {color :: T.Text, x :: Int, y :: Int} deriving (Eq, Show)

$(deriveJSON defaultOptions ''Stone)

data Point = Point {x :: Int, y :: Int} deriving (Eq, Show)

$(deriveJSON defaultOptions ''Point)

data UndoResponse = UndoResponse {add :: [Stone], remove :: [Point]} deriving (Eq, Show)

$(deriveJSON defaultOptions ''UndoResponse)

selectRecord :: S.Statement (Int64, Int64) Int64
selectRecord =
  [TH.singletonStatement|
    select
    board_size :: int8
    from record_record
    where
    owner_id = $1 :: int8 and id = $2 :: int8
  |]

type MoveRow = (Int64, Maybe Int64, T.Text)

selectMoves :: S.Statement Int64 (V.Vector MoveRow)
selectMoves =
  [TH.vectorStatement|
    select
    move :: int8, position :: int8?, color :: text
    from record_move
    where
    record_id = $1 :: int8
    order by move asc
  |]

deleteMove :: S.Statement (Int64, Int64) Int64
deleteMove =
  [TH.rowsAffectedStatement|
    delete from record_move where
    record_id = $1 :: int8 and move = $2 :: int8
    |]

toColor :: T.Text -> Color
toColor color' = case color' of
  "B" -> Black
  "W" -> White
  _ -> White -- TODO????

fromColor :: Color -> T.Text
fromColor Black = "B"
fromColor White = "W"

invert :: Color -> Color
invert Black = White
invert White = Black

undo :: HP.Pool -> ScottyM ()
undo pool = post "/api/records/:recordId/undo/" $ do
  userId <- authorizedUserId
  recordId <- param "recordId"
  size' <- execute pool selectRecord (userId, recordId)
  moves' <- execute pool selectMoves recordId
  let size = fromIntegral size'
      (moveNumber, pos, color) = V.last moves'
      removals = case pos of
        Just pos' -> do
          let (x, y) = toCoord' size $ fromIntegral pos'
          [Point {x, y}]
        Nothing -> []
  case [(fmap fromIntegral pos', toColor color') | (_, pos', color') <- V.toList moves'] of
    [] -> raiseStatus status403 "No moves to undo"
    moves -> do
      case runBoardA size $ playStones moves of
        Right (_, captures) -> do
          let additions =
                [ do
                    let (x, y) = toCoord' size pos'
                    Stone {x, y, color = fromColor $ invert $ toColor color}
                  | pos' <- Set.toAscList captures
                ]
          _ <- execute pool deleteMove (recordId, moveNumber)
          status status200
          json UndoResponse {add = additions, remove = removals}
        Left _ -> raiseStatus status500 "Error replaying game"
