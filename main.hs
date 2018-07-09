module Main where

type Row = Int
type Col = Int
data ChessPiece = Rook Row Col | Bishop Row Col | Knight Row Col | Queen Row Col | King Row Col

class Piece p where
    attacks :: p -> Bool

instance Show ChessPiece where
    show (Rook _ _) = "R"
    show (Bishop _ _) = "B"
    show (Knight _ _) = "N"
    show (Queen _ _) = "Q"
    show (King _ _) = "K"

rookMoves :: ChessPiece -> [(Row, Col)]
rookMoves (Rook row col) = take (length ([ (row+x,col+y) | x <- [-1,1,0], y <-[-1,1,0]]) - 1) [ (row+x,col+y) | x <- [-1,1,0], y <-[-1,1,0]]

instance Piece ChessPiece where
    attacks (Rook row col) = elem (row, col) (rookMoves (Rook row col))
    attacks (Bishop row col) = False
    attacks (Knight row col) = False
    attacks (Queen row col) = False
    attacks (King row col) = False

main = do
    putStrLn "Ahoy there! What name?"
    name <- getLine
    putStrLn $ "Hey there  " ++ name