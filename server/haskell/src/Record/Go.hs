{-# HLINT ignore "Use tuple-section" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Record.Go
  ( BoardS,
    BoardA,
    GoError (..),
    Pos,
    Coord,
    Color (White, Black),
    boardSize,
    toCoord,
    toPos,
    adjacents,
    runBoardA,
    buildGroup',
    buildGroup,
    attemptMurder,
    placeStone,
    playStones,
  )
where

import Control.Monad (foldM, when)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.Reader (MonadReader (ask), Reader, runReader)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe (isJust, isNothing)
import Data.Set (Set)
import qualified Data.Set as Set

data Color = White | Black deriving (Eq, Show)

type Pos = Int

type Coord = (Int, Int)

-- Map the Pos to the color of the stone occupying that space
type Board = IntMap Color

-- The board size
type BoardS = Int

data GoError = OutOfBounds Pos | SpaceOccupied Coord | Suicide Coord deriving (Eq, Show)

type BoardA = ExceptT GoError (Reader BoardS)

-- instance MonadError GoError (ExceptT GoError (Reader BoardS))

runBoardA :: Int -> BoardA a -> Either GoError a
runBoardA size action = runReader (runExceptT action) size

boardSize :: BoardA Int
boardSize = do ask

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

buildGroup' :: Board -> Color -> Set Int -> Set Int -> [Coord] -> BoardA (Set Int, Set Int)
buildGroup' board color group liberties coordsToCheck
  | null coordsToCheck = return (group, liberties)
  | otherwise = do
      let coord = head coordsToCheck
      pos <- toPos coord
      if pos `Set.member` group || pos `Set.member` liberties
        then buildGroup' board color group liberties $ tail coordsToCheck
        else do
          let stone = board IntMap.!? pos
          adjs <- adjacents coord
          let (group', coordsToCheck') =
                if stone == Just color
                  then (Set.insert pos group, tail coordsToCheck ++ adjs)
                  else (group, tail coordsToCheck)
          let liberties' =
                if isNothing stone
                  then Set.insert pos liberties
                  else liberties
          buildGroup' board color group' liberties' coordsToCheck'

buildGroup :: Board -> Coord -> BoardA (Set Int, Set Int)
buildGroup board coord = do
  pos <- toPos coord
  let intersection = board IntMap.!? pos
  case intersection of
    Just color -> buildGroup' board color group liberties coordsToCheck
    Nothing -> return (group, liberties)
  where
    group = Set.empty
    liberties = Set.empty
    coordsToCheck = [coord]

attemptMurder :: Board -> Coord -> BoardA Board
attemptMurder board coord = do
  (group, liberties) <- buildGroup board coord
  return $
    if Set.null liberties
      then IntMap.filterWithKey (\pos _ -> not $ Set.member pos group) board
      else board

placeStone :: Board -> (Pos, Color) -> BoardA Board
placeStone board (pos, color) = do
  -- TODO check if move is suicidal
  size <- boardSize
  when (pos < 0 || pos >= size * size) $ throwError $ OutOfBounds pos
  coord <- toCoord pos
  when (isJust $ board IntMap.!? pos) $ throwError $ SpaceOccupied coord
  adjs <- adjacents coord
  let addedStone = IntMap.insert pos color board
  killedBoard <- foldM attemptMurder addedStone adjs
  (_, liberties) <- buildGroup killedBoard coord
  when (Set.null liberties) $ throwError $ Suicide coord
  return killedBoard

playStones :: [(Pos, Color)] -> BoardA Board
playStones = foldM placeStone IntMap.empty
