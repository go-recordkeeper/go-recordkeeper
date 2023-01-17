module Record.GoSpec (spec) where

import Record.Go (BoardA, boardSize, runBoardA, toCoord, toPos)
import Test.Hspec

withBoardSizes :: BoardA Expectation -> Expectation
withBoardSizes test = mapM_ (`runBoardA` test) [9, 13, 19]

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
