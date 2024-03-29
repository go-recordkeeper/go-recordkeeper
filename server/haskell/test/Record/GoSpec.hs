module Record.GoSpec (spec) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set as Set
import Record.Go (BoardA, Color (Black, White), GoError (OutOfBounds, SpaceOccupied, Suicide), adjacents, boardSize, identifyCaptures, placeStone, playStones, runBoardA, toCoord, toPos)
import Test.Hspec

withBoardSizes :: BoardA Expectation -> Expectation
withBoardSizes test = mapM_ (\size -> noError $ runBoardA size test) [9, 13, 19]

noError :: Either GoError a -> a
noError (Left e) = error $ show e
noError (Right a') = a'

expectError :: GoError -> Either GoError a -> Expectation
expectError expected (Left actual) = expected `shouldBe` actual
expectError _ (Right _) = error "Test did not fail"

spec :: Spec
spec = describe "Record.Go" $ do
  it "converts pos to coord to pos" $ withBoardSizes $ do
    size <- boardSize
    sequence_
      <$> sequence
        [ do
            coord <- toCoord pos
            pos' <- toPos coord
            return $ pos' `shouldBe` pos
          | pos <- [0 .. (size * size) - 1]
        ]

  it "converts coord to pos to coord" $ withBoardSizes $ do
    size <- boardSize
    sequence_
      <$> sequence
        [ do
            pos <- toPos (x, y)
            coord' <- toCoord pos
            return $ coord' `shouldBe` (x, y)
          | x <- [0 .. size - 1],
            y <- [0 .. size - 1]
        ]

  it "calculates correct adjacents for corners" $ noError $ runBoardA 9 $ do
    topLeft <- adjacents (0, 0)
    topRight <- adjacents (8, 0)
    bottomLeft <- adjacents (0, 8)
    bottomRight <- adjacents (8, 8)
    return $
      sequence_
        [ topLeft `shouldBe` [(1, 0), (0, 1)],
          topRight `shouldBe` [(7, 0), (8, 1)],
          bottomLeft `shouldBe` [(1, 8), (0, 7)],
          bottomRight `shouldBe` [(7, 8), (8, 7)]
        ]

  it "calculates correct adjacents for edges" $ withBoardSizes $ do
    topAdjs <- adjacents (1, 0)
    leftAdjs <- adjacents (0, 1)
    -- TODO other sides
    return $
      sequence_
        [ topAdjs `shouldBe` [(0, 0), (2, 0), (1, 1)],
          leftAdjs `shouldBe` [(1, 1), (0, 0), (0, 2)]
        ]

  it "calculates correct adjacents in the middle" $ withBoardSizes $ do
    adjs <- adjacents (1, 1)
    return $ adjs `shouldBe` [(0, 1), (2, 1), (1, 0), (1, 2)]

  it "places a stone" $ withBoardSizes $ do
    (board, captures) <- placeStone IntMap.empty (Just 0, Black)
    return $
      sequence_
        [ IntMap.size board `shouldBe` 1,
          (board IntMap.!? 0) `shouldBe` Just Black,
          captures `shouldBe` Set.empty
        ]

  it "cannot place a stone at -1" $ expectError (OutOfBounds (-1)) $ runBoardA 9 $ do
    placeStone IntMap.empty (Just (-1), Black)

  it "cannot place a stone beyond the board" $ expectError (OutOfBounds 81) $ runBoardA 9 $ do
    placeStone IntMap.empty (Just 81, Black)

  it "plays two stones" $ withBoardSizes $ do
    (board, captures) <- playStones [(Just 0, Black), (Just 1, White)]
    return $
      sequence_
        [ IntMap.size board `shouldBe` 2,
          (board IntMap.!? 0) `shouldBe` Just Black,
          (board IntMap.!? 1) `shouldBe` Just White,
          captures `shouldBe` Set.empty
        ]

  it "captures a stone" $ noError $ runBoardA 9 $ do
    (board, captures) <- playStones [(Just 0, Black), (Just 1, White), (Just 10, Black), (Just 9, White)]
    return $
      sequence_
        [ IntMap.size board `shouldBe` 3,
          (board IntMap.!? 0) `shouldBe` Nothing,
          (board IntMap.!? 1) `shouldBe` Just White,
          (board IntMap.!? 9) `shouldBe` Just White,
          (board IntMap.!? 10) `shouldBe` Just Black,
          captures `shouldBe` Set.fromList [0]
        ]

  it "captures a group" $ noError $ runBoardA 9 $ do
    (board, captures) <- playStones [(Just 0, Black), (Just 9, White), (Just 1, Black), (Just 10, White), (Just 2, Black), (Just 11, White), (Just 12, Black), (Just 3, White)]
    return $
      sequence_
        [ IntMap.size board `shouldBe` 5,
          (board IntMap.!? 0) `shouldBe` Nothing,
          (board IntMap.!? 1) `shouldBe` Nothing,
          (board IntMap.!? 2) `shouldBe` Nothing,
          (board IntMap.!? 3) `shouldBe` Just White,
          (board IntMap.!? 9) `shouldBe` Just White,
          (board IntMap.!? 10) `shouldBe` Just White,
          (board IntMap.!? 11) `shouldBe` Just White,
          (board IntMap.!? 12) `shouldBe` Just Black,
          captures `shouldBe` Set.fromList [0, 1, 2]
        ]

  it "captures suicidally" $ noError $ runBoardA 9 $ do
    -- Black plays at X
    -- XWB
    -- WB
    (board, captures) <- playStones [(Just 2, Black), (Just 1, White), (Just 10, Black), (Just 9, White), (Just 0, Black)]
    return $
      sequence_
        [ IntMap.size board `shouldBe` 4,
          (board IntMap.!? 0) `shouldBe` Just Black,
          (board IntMap.!? 1) `shouldBe` Nothing,
          (board IntMap.!? 2) `shouldBe` Just Black,
          (board IntMap.!? 9) `shouldBe` Just White,
          (board IntMap.!? 10) `shouldBe` Just Black,
          captures `shouldBe` Set.fromList [1]
        ]

  it "cannot play on top of another stone" $ expectError (SpaceOccupied (0, 0)) $ runBoardA 9 $ do
    playStones [(Just 0, Black), (Just 0, White)]

  it "cannot commit suicide" $ expectError (Suicide (0, 0)) $ runBoardA 9 $ do
    playStones [(Just 1, White), (Just 9, White), (Just 0, Black)]

  it "cannot suicide a whole group" $ expectError (Suicide (0, 0)) $ runBoardA 9 $ do
    playStones [(Just 1, Black), (Just 9, White), (Just 10, White), (Just 2, White), (Just 0, Black)]

  it "identifies a capture" $ noError $ runBoardA 9 $ do
    (movesAndCaptures, _) <- identifyCaptures [(Just 1, Black), (Just 0, White), (Just 9, Black)]
    return $ movesAndCaptures `shouldBe` [((Just 1, Black), Set.fromList []), ((Just 0, White), Set.fromList []), ((Just 9, Black), Set.fromList [0])]
