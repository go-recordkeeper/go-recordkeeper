module Record.Pass (pass) where

import Auth.JWT (authorizedUserId)
import Control.Monad.IO.Class (liftIO)
import Data.Int (Int64)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Hasql.Pool as HP
import qualified Hasql.Session as HS
import qualified Hasql.Statement as S
import qualified Hasql.TH as TH
import Network.HTTP.Types (status201, status404, status500)
import Record.Go (Color (Black, White))
import Web.Scotty
  ( ScottyM,
    param,
    post,
    raiseStatus,
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
  recordSelect <- liftIO $ HP.use pool $ HS.statement (userId, recordId) selectRecord
  movesSelect <- liftIO $ HP.use pool $ HS.statement recordId selectMoves
  case (recordSelect, movesSelect) of
    (Right handicap', Right moves') -> do
      let moves = map toColor $ V.toList moves'
          handicap = fromIntegral handicap'
          moveColor = nextColor handicap moves
      result <- liftIO $ HP.use pool $ HS.statement (fromIntegral recordId, fromColor moveColor, fromIntegral $ 1 + length moves) insertMove
      case result of
        Right _ -> do
          status status201
        Left _ -> do
          raiseStatus status500 "DB error"
    (Left (HP.SessionError (HS.QueryError _ _ (HS.ResultError (HS.UnexpectedAmountOfRows 0)))), _) -> do
      raiseStatus status404 "Does not exist."
    (_, _) -> do
      raiseStatus status500 "DB error"
