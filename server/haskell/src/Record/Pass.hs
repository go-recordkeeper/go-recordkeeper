module Record.Pass (pass) where

import Auth.JWT (authorizedUserId)
import DB (execute)
import Data.Int (Int64)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Hasql.Pool as HP
import qualified Hasql.Statement as S
import qualified Hasql.TH as TH
import Network.HTTP.Types (status201)
import Record.Go (Color (Black, White))
import Web.Scotty
  ( ScottyM,
    param,
    post,
    status,
  )

selectRecord :: S.Statement (Int64, Int64) Int64
selectRecord =
  [TH.singletonStatement|
    select
    handicap :: int8
    from record_record
    where
    owner_id = $1 :: int8 and id = $2 :: int8
  |]

type MoveRow = T.Text

selectMoves :: S.Statement Int64 (V.Vector MoveRow)
selectMoves =
  [TH.vectorStatement|
    select
    color :: text
    from record_move
    where
    record_id = $1 :: int8
  |]

type InsertMoveRow = (Int64, T.Text, Int64)

insertMove :: S.Statement InsertMoveRow Int64
insertMove =
  [TH.rowsAffectedStatement|
    insert into record_move
    (record_id, position, color, move)
    values
    ($1 :: int8, NULL, $2 :: text, $3 :: int8)
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

nextColor :: Int -> [Color] -> Color
nextColor handicap colors =
  if null colors || length colors < handicap || last colors == White
    then Black
    else White

pass :: HP.Pool -> ScottyM ()
pass pool = post "/api/records/:recordId/pass/" $ do
  userId <- authorizedUserId
  recordId <- param "recordId"
  handicap' <- execute pool selectRecord (userId, recordId)
  moves' <- execute pool selectMoves recordId
  let handicap = fromIntegral handicap'
      moves = map toColor $ V.toList moves'
      moveColor = nextColor handicap moves
  _ <- execute pool insertMove (recordId, fromColor moveColor, fromIntegral $ 1 + length moves)
  status status201
