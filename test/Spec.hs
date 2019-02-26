import Test.Hspec
import Lib
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
  describe "Chesspiece Attacks" $ do
    it "test King movements" $ do
      attacks Piece{row=5,col=5,piece=King} Piece{row=5,col=6,piece=King}  `shouldBe` True
      attacks Piece{row=5,col=5,piece=King} Piece{row=6,col=4,piece=King}  `shouldBe` True
      attacks Piece{row=5,col=5,piece=King} Piece{row=5,col=4,piece=King}  `shouldBe` True
      attacks Piece{row=5,col=5,piece=King} Piece{row=4,col=4,piece=King}  `shouldBe` True
      attacks Piece{row=5,col=5,piece=King} Piece{row=4,col=5,piece=King}  `shouldBe` True
      attacks Piece{row=5,col=5,piece=King} Piece{row=4,col=6,piece=King}  `shouldBe` True
      attacks Piece{row=5,col=5,piece=King} Piece{row=6,col=6,piece=King}  `shouldBe` True
      attacks Piece{row=5,col=5,piece=King} Piece{row=8,col=8,piece=King}  `shouldBe` False

    it "test Bishop movements" $ do
        attacks Piece{row=5,col=5,piece=Bishop} Piece{row=7,col=7,piece=Bishop}  `shouldBe` True
        attacks Piece{row=5,col=5,piece=Bishop} Piece{row=6,col=6,piece=Bishop}  `shouldBe` True
        attacks Piece{row=5,col=5,piece=Bishop} Piece{row=4,col=6,piece=Bishop}  `shouldBe` True
        attacks Piece{row=5,col=5,piece=Bishop} Piece{row=3,col=7,piece=Bishop}  `shouldBe` True
        attacks Piece{row=5,col=5,piece=Bishop} Piece{row=7,col=3,piece=Bishop}  `shouldBe` True
        attacks Piece{row=5,col=5,piece=Bishop} Piece{row=6,col=4,piece=Bishop}  `shouldBe` True
        attacks Piece{row=5,col=5,piece=Bishop} Piece{row=4,col=4,piece=Bishop}  `shouldBe` True
        attacks Piece{row=5,col=5,piece=Bishop} Piece{row=3,col=3,piece=Bishop}  `shouldBe` True
        attacks Piece{row=5,col=5,piece=Bishop} Piece{row=8,col=1,piece=Bishop}  `shouldBe` False

    it "test Knigh movements" $ do
        attacks Piece{row=5,col=5,piece=Knight} Piece{row=7,col=6,piece=Knight}  `shouldBe` True
        attacks Piece{row=5,col=5,piece=Knight} Piece{row=6,col=7,piece=Knight}  `shouldBe` True
        attacks Piece{row=5,col=5,piece=Knight} Piece{row=4,col=7,piece=Knight}  `shouldBe` True
        attacks Piece{row=5,col=5,piece=Knight} Piece{row=3,col=6,piece=Knight}  `shouldBe` True
        attacks Piece{row=5,col=5,piece=Knight} Piece{row=3,col=4,piece=Knight}  `shouldBe` True
        attacks Piece{row=5,col=5,piece=Knight} Piece{row=4,col=3,piece=Knight}  `shouldBe` True
        attacks Piece{row=5,col=5,piece=Knight} Piece{row=6,col=3,piece=Knight}  `shouldBe` True
        attacks Piece{row=5,col=5,piece=Knight} Piece{row=8,col=1,piece=Knight}  `shouldBe` False

    it "test Queen movements" $ do
        attacks Piece{row=5,col=5,piece=Queen} Piece{row=7,col=7,piece=Queen}  `shouldBe` True
        attacks Piece{row=5,col=5,piece=Queen} Piece{row=6,col=6,piece=Queen}  `shouldBe` True
        attacks Piece{row=5,col=5,piece=Queen} Piece{row=4,col=6,piece=Queen}  `shouldBe` True
        attacks Piece{row=5,col=5,piece=Queen} Piece{row=3,col=7,piece=Queen}  `shouldBe` True
        attacks Piece{row=5,col=5,piece=Queen} Piece{row=7,col=3,piece=Queen}  `shouldBe` True
        attacks Piece{row=5,col=5,piece=Queen} Piece{row=6,col=4,piece=Queen}  `shouldBe` True
        attacks Piece{row=5,col=5,piece=Queen} Piece{row=4,col=4,piece=Queen}  `shouldBe` True
        attacks Piece{row=5,col=5,piece=Queen} Piece{row=3,col=3,piece=Queen}  `shouldBe` True
        attacks Piece{row=5,col=5,piece=Queen} Piece{row=6,col=5,piece=Queen}  `shouldBe` True
        attacks Piece{row=5,col=5,piece=Queen} Piece{row=7,col=5,piece=Queen}  `shouldBe` True
        attacks Piece{row=5,col=5,piece=Queen} Piece{row=4,col=5,piece=Queen}  `shouldBe` True
        attacks Piece{row=5,col=5,piece=Queen} Piece{row=3,col=5,piece=Queen}  `shouldBe` True
        attacks Piece{row=5,col=5,piece=Queen} Piece{row=5,col=6,piece=Queen}  `shouldBe` True
        attacks Piece{row=5,col=5,piece=Queen} Piece{row=5,col=7,piece=Queen}  `shouldBe` True
        attacks Piece{row=5,col=5,piece=Queen} Piece{row=5,col=4,piece=Queen}  `shouldBe` True
        attacks Piece{row=5,col=5,piece=Queen} Piece{row=5,col=3,piece=Queen}  `shouldBe` True
        attacks Piece{row=5,col=5,piece=Queen} Piece{row=3,col=4,piece=Queen}  `shouldBe` False

    it "test Rook movements" $ do
        attacks Piece{row=5,col=5,piece=Rook} Piece{row=6,col=5,piece=Rook}  `shouldBe` True
        attacks Piece{row=5,col=5,piece=Rook} Piece{row=7,col=5,piece=Rook}  `shouldBe` True
        attacks Piece{row=5,col=5,piece=Rook} Piece{row=4,col=5,piece=Rook}  `shouldBe` True
        attacks Piece{row=5,col=5,piece=Rook} Piece{row=3,col=5,piece=Rook}  `shouldBe` True
        attacks Piece{row=5,col=5,piece=Rook} Piece{row=5,col=6,piece=Rook}  `shouldBe` True
        attacks Piece{row=5,col=5,piece=Rook} Piece{row=5,col=7,piece=Rook}  `shouldBe` True
        attacks Piece{row=5,col=5,piece=Rook} Piece{row=5,col=4,piece=Rook}  `shouldBe` True
        attacks Piece{row=5,col=5,piece=Rook} Piece{row=5,col=3,piece=Rook}  `shouldBe` True
        attacks Piece{row=5,col=5,piece=Rook} Piece{row=7,col=2,piece=Rook}  `shouldBe` False
