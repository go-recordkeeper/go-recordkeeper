module Record.GoSpec (spec) where

import Record.Go (toCoord', toPos')
import Test.Hspec
import Test.QuickCheck

withBoardSize :: (Int -> Expectation) -> Expectation
withBoardSize test = mapM_ test [9, 13, 19]

spec :: Spec
spec = describe "Record.Go" $ do
  it "converts" $ do
    let boardSize = 9
    property $ \pos -> toPos' boardSize (toCoord' boardSize pos) == pos

  it "converts pos to coord to pos" $ do
    withBoardSize $ \boardSize -> mapM_ (\pos -> toPos' boardSize (toCoord' boardSize pos) `shouldBe` pos) [0 .. (boardSize * boardSize) - 1]
  it "converts coord to pos to coord" $ do
    withBoardSize $ \boardSize ->
      mapM_
        ( \x ->
            mapM_
              ( \y ->
                  toCoord' boardSize (toPos' boardSize (x, y)) `shouldBe` (x, y)
              )
              [0 .. boardSize - 1]
        )
        [0 .. boardSize - 1]
