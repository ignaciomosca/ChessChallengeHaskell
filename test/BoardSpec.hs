module BoardSpec where

import Test.Hspec
import Lib
import Control.Exception (evaluate)
import Data.Set
import Data.Set.Extra

spec :: Spec
spec = do
  describe "Board Tests" $ do
    it "4x4 2K 2Q" $ do
      length(toList(makeSolution 4 4 2 2 0 0 0)) `shouldBe` 20
    it "4x4 1Q 2R" $ do
      length(toList(makeSolution 4 4 0 1 0 2 0)) `shouldBe` 116
    it "4x4 2K 2N" $ do
      length(toList(makeSolution 4 4 2 0 0 0 2)) `shouldBe` 322
    it "4x4 2Q 1B" $ do
      length(toList(makeSolution 4 4 0 2 1 0 0)) `shouldBe` 72
    it "5x5 2K 2Q" $ do
      length(toList(makeSolution 5 5 2 2 0 0 0)) `shouldBe` 816
    it "5x5 1Q 2B" $ do
      length(toList(makeSolution 5 5 0 1 2 0 0)) `shouldBe` 1152
    it "5x5 1Q 1B 1R" $ do
      length(toList(makeSolution 5 5 0 1 1 1 0)) `shouldBe` 1224
    it "5x5 2K 2B" $ do
      length(toList(makeSolution 5 5 2 0 2 0 0)) `shouldBe` 7596
    it "6x6 2K 1B 1N 1R" $ do
      length(toList(makeSolution 6 6 2 0 1 1 1)) `shouldBe` 343816
    it "7x7 2K 2Q 2B 0R 1N" $ do
      length(toList(makeSolution 7 7 2 2 2 0 1)) `shouldBe` 3063828
