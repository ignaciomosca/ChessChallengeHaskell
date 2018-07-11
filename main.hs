module Main where

data PieceType = Rook | Bishop | Knight | Queen | King deriving(Show)
data ChessPiece = Piece {row::Int, col::Int, piece::PieceType} deriving(Show)

class Piece p where
    attacks :: p -> ChessPiece -> Bool

instance Piece ChessPiece where
    attacks Piece{row = aRow, col = aCol, piece = Rook} Piece{row = r, col = c , piece = _} = aRow == r || aCol == c

main = do
    putStrLn "Ahoy there! What name?"
    name <- getLine
    putStrLn $ "Hey there  " ++ name