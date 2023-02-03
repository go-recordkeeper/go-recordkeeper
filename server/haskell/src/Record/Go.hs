{-# HLINT ignore "Use tuple-section" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Record.Go
  ( BoardS,
    BoardA,
    GoError (..),
    Pos,
    Coord,
    Color (White, Black),
    Move,
    boardSize,
    toCoord',
    toCoord,
    toPos',
    toPos,
    adjacents,
    runBoardA,
    buildGroup',
    buildGroup,
    placeStone,
    playStones,
    identifyCaptures,
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

type Move = (Maybe Pos, Color)

type Group = Set Pos

-- Map the Pos to the color of the stone occupying that space
type Board = IntMap Color

-- The board size
type BoardS = Int

data GoError = OutOfBounds Pos | SpaceOccupied Coord | Suicide Coord deriving (Eq, Show)

type BoardA = ExceptT GoError (Reader BoardS)

inverse :: Color -> Color
inverse Black = White
inverse White = Black

runBoardA :: Int -> BoardA a -> Either GoError a
runBoardA size action = runReader (runExceptT action) size

boardSize :: BoardA Int
boardSize = do ask

toCoord' :: Int -> Pos -> Coord
toCoord' size pos = (mod pos size, div pos size)

toCoord :: Pos -> BoardA Coord
toCoord pos = do
  size <- boardSize
  return $ toCoord' size pos

toPos' :: Int -> Coord -> Pos
toPos' size (x, y) = (y * size) + x

toPos :: Coord -> BoardA Pos
toPos coord = do
  size <- boardSize
  return $ toPos' size coord

adjacents :: Coord -> BoardA [Coord]
adjacents (x, y) = do
  size <- boardSize
  return [(x', y') | (x', y') <- [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)], 0 <= x', x' < size, 0 <= y', y' < size]

buildGroup' :: Board -> Color -> Group -> Group -> [Coord] -> BoardA (Group, Group)
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

buildGroup :: Board -> Coord -> BoardA (Group, Group)
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

deadGroup :: Color -> Board -> Coord -> BoardA Group
deadGroup color board coord = do
  (group, liberties) <- buildGroup board coord
  pos <- toPos coord
  return $
    if board IntMap.!? pos == Just color && Set.null liberties
      then group
      else Set.empty

placeStone :: Board -> Move -> BoardA (Board, Group)
placeStone board (Nothing, _) = return (board, Set.empty)
placeStone board (Just pos, color) = do
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
          captures'' <- deadGroup (inverse color) boardWithStone adj
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

playStones :: [Move] -> BoardA (Board, Group)
playStones = foldM (\(board, _) -> placeStone board) (IntMap.empty, Set.empty)

identifyCaptures :: [Move] -> BoardA ([(Move, Group)], Board)
identifyCaptures moves = do
  foldM
    ( \(previousMoves, board) move' -> do
        (board', captures) <- placeStone board move'
        return (previousMoves ++ [(move', captures)], board')
    )
    ([], IntMap.empty)
    moves
