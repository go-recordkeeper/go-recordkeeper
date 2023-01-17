{-# HLINT ignore "Use tuple-section" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Record.Go
  ( BoardS,
    BoardA,
    boardSize,
    toCoord,
    toPos,
    adjacents,
    runBoardA,
  )
where

import Control.Monad (foldM)
import Control.Monad.Reader (MonadReader (ask), Reader, runReader)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe (isNothing)
import Data.Set (Set)
import qualified Data.Set as Set

data Color = White | Black deriving (Eq)

type Pos = Int

type Coord = (Int, Int)

-- Map the Pos to the color of the stone occupying that space
type Board = IntMap Color

-- Several functions need the board and it's size, so wrap that into a single type
newtype BoardS = BoardS {boardState :: (Int, Board)}

type BoardA = Reader BoardS

runBoardA :: Int -> BoardA a -> a
runBoardA size action = runReader action $ BoardS (size, IntMap.empty)

boardSize :: BoardA Int
boardSize = do
  BoardS (size, _) <- ask
  return size

board :: BoardA Board
board = do
  BoardS (_, board') <- ask
  return board'

toCoord :: Pos -> BoardA Coord
toCoord pos = do
  size <- boardSize
  return (mod pos size, div pos size)

toPos :: Coord -> BoardA Pos
toPos (x, y) = do
  size <- boardSize
  return $ (y * size) + x

adjacents :: Coord -> BoardA [Coord]
adjacents (x, y) = do
  size <- boardSize
  return [(x', y') | (x', y') <- [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)], 0 <= x', x' < size, 0 <= y', y' < size]

buildGroup' :: Color -> Set Int -> Set Int -> [Coord] -> BoardA (Set Int, Set Int)
buildGroup' color group liberties coordsToCheck
  | null coordsToCheck = return (group, liberties)
  | otherwise = do
      let coord = head coordsToCheck
      pos <- toPos coord
      b <- board
      let stone = b IntMap.!? pos
      adjs <- adjacents coord
      let (group', coordsToCheck') =
            if stone == Just color
              then (Set.insert pos group, tail coordsToCheck ++ adjs)
              else (group, tail coordsToCheck)
      let liberties' =
            if isNothing stone
              then Set.insert pos liberties
              else liberties
      buildGroup' color group' liberties' coordsToCheck'

buildGroup :: Coord -> BoardA (Set Int, Set Int)
buildGroup coord = do
  b <- board
  pos <- toPos coord
  let intersection = b IntMap.!? pos
  case intersection of
    Just color -> buildGroup' color group liberties coordsToCheck
    Nothing -> return (group, liberties)
  where
    group = Set.empty
    liberties = Set.empty
    coordsToCheck = [coord]

attemptMurder :: Coord -> BoardA Board
attemptMurder coord = do
  b <- board
  (group, liberties) <- buildGroup coord
  return $
    if Set.null liberties
      then IntMap.filterWithKey (\pos _ -> not $ Set.member pos group) b
      else b

placeStone :: Pos -> Color -> BoardA Board
placeStone pos color = do
  -- TODO check if space is already occupied
  -- TODO check if move is suicidal
  coord <- toCoord pos
  adjs <- adjacents coord
  b <- board
  let addedStone = IntMap.insert pos color b
  foldM (const attemptMurder) addedStone adjs
