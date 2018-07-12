module Main where

data PieceType = Rook | Bishop | Knight | Queen | King deriving(Show)
data ChessPiece = Piece {row::Int, col::Int, piece::PieceType} deriving(Show)
class Piece p where
    attacks :: p -> ChessPiece -> Bool

kingMoves::Int->Int->[(Int,Int)]
kingMoves r c = [ (r+x,c+y) | x <- [-1,1,0], y <-[-1,1,0]]

knightMoves::Int->Int->[(Int,Int)]
knightMoves r c = [ (r+x,c+y) | x <- [-2,-1,1,2], y <-[-2,-1,1,2], abs x /= abs y]

instance Piece ChessPiece where
    attacks Piece{row = aRow, col = aCol, piece = Rook} Piece{row = r, col = c} = aRow == r || aCol == c
    attacks Piece{row = aRow, col = aCol, piece = Bishop} Piece{row = r, col = c} = abs(r - aRow) == abs(c - aCol)
    attacks Piece{row = aRow, col = aCol, piece = Knight} Piece{row = r, col = c} = elem (r,c) (knightMoves aRow aCol)
    attacks Piece{row = aRow, col = aCol, piece = Queen} Piece{row = r, col = c} = r == aRow || c == aCol || abs(r - aRow) == abs(c - aCol)
    attacks Piece{row = aRow, col = aCol, piece = King} Piece{row = r, col = c} = elem (r,c) (kingMoves aRow aCol)

main = do
    putStrLn "Ahoy there! What name?"
    name <- getLine
    putStrLn $ "Hey there  " ++ name