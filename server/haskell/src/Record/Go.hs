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

buildGroup' :: Board -> Color -> Set Pos -> Set Pos -> [Coord] -> BoardA (Set Pos, Set Pos)
buildGroup' _ _ group liberties [] = return (group, liberties)
buildGroup' board color group liberties (coord : coordsToCheck) = do
  pos <- toPos coord
  if pos `Set.member` group || pos `Set.member` liberties
    then buildGroup' board color group liberties coordsToCheck
    else do
      let stone = board IntMap.!? pos
      adjs <- adjacents coord
      let (group', coordsToCheck') =
            if stone == Just color
              then (Set.insert pos group, coordsToCheck ++ adjs)
              else (group, coordsToCheck)
      let liberties' =
            if isNothing stone
              then Set.insert pos liberties
              else liberties
      buildGroup' board color group' liberties' coordsToCheck'

buildGroup :: Board -> Coord -> BoardA (Set Pos, Set Pos)
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

deadGroup :: Board -> Coord -> BoardA (Set Pos)
deadGroup board coord = do
  (group, liberties) <- buildGroup board coord
  return $
    if Set.null liberties
      then group
      else Set.empty

placeStone :: Board -> (Pos, Color) -> BoardA (Board, Set Pos)
placeStone board (pos, color) = do
  size <- boardSize
  -- Test that the position is on the board
  when (pos < 0 || pos >= size * size) $ throwError $ OutOfBounds pos
  coord <- toCoord pos
  -- Test that there are no stones already at the desired location
  when (isJust $ board IntMap.!? pos) $ throwError $ SpaceOccupied coord
  adjs <- adjacents coord
  let boardWithStone = IntMap.insert pos color board
  -- Collect all captured groups into a single set
  captures <-
    foldM
      ( \captures' adj -> do
          captures'' <- deadGroup boardWithStone adj
          return $ Set.union captures' captures''
      )
      Set.empty
      adjs
  -- Remove captured stones from the board
  let culledBoard = IntMap.filterWithKey (\p _ -> not $ Set.member p captures) boardWithStone
  -- Test if the newly placed stone has any liberties
  (_, liberties) <- buildGroup culledBoard coord
  when (Set.null captures && Set.null liberties) $ throwError $ Suicide coord
  return (culledBoard, captures)

playStones :: [(Pos, Color)] -> BoardA (Board, Set Pos)
playStones = foldM (\(board, _) -> placeStone board) (IntMap.empty, Set.empty)
