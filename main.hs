module Main where

import Data.Set (Set)

data PieceType = Rook | Bishop | Knight | Queen | King deriving(Show, Eq, Ord)
data ChessPiece = Piece {row::Int, col::Int, piece::PieceType} deriving(Eq, Ord)
data Board = Board{m:: Int, n:: Int, usedPieces:: Set ChessPiece , numberOfPieces:: Int} deriving(Eq, Ord)
class Piece piece where
    attacks :: piece -> ChessPiece -> Bool

class ChessBoard board where
    isSafe :: board -> ChessPiece->Bool
    place :: board -> ChessPiece->Board
    findChessPiece::board->Int->Int->String
    done::board->Bool

kingMoves::Int->Int->[(Int,Int)]
kingMoves r c = [ (r+x,c+y) | x <- [-1,1,0], y <-[-1,1,0]]

knightMoves::Int->Int->[(Int,Int)]
knightMoves r c = [ (r+x,c+y) | x <- [-2,-1,1,2], y <-[-2,-1,1,2], abs x /= abs y]

instance ChessBoard Board where
    isSafe Board{usedPieces=up} c = all (\p -> attacks c p == attacks p c && attacks p c == False) up

instance Show ChessPiece where
    show Piece{row = _, col = _, piece = Rook} = "R"
    show Piece{row = _, col = _, piece = Bishop} = "B"
    show Piece{row = _, col = _, piece = Knight} = "N"
    show Piece{row = _, col = _, piece = Queen} = "Q"
    show Piece{row = _, col = _, piece = King} = "K"

instance Piece ChessPiece where
    attacks Piece{row = aRow, col = aCol, piece = Rook} Piece{row = r, col = c} = aRow == r || aCol == c
    attacks Piece{row = aRow, col = aCol, piece = Bishop} Piece{row = r, col = c} = abs(r - aRow) == abs(c - aCol)
    attacks Piece{row = aRow, col = aCol, piece = Knight} Piece{row = r, col = c} = elem (r,c) (knightMoves aRow aCol)
    attacks Piece{row = aRow, col = aCol, piece = Queen} Piece{row = r, col = c} = r == aRow || c == aCol || abs(r - aRow) == abs(c - aCol)
    attacks Piece{row = aRow, col = aCol, piece = King} Piece{row = r, col = c} = elem (r,c) (kingMoves aRow aCol)

main = do
    putStrLn "How many kings are to be placed on the board?"
    kings <- getLine
    putStrLn "How many queens are to be placed on the board?"
    queens <- getLine
    putStrLn "How many bishops are to be placed on the board?"
    bishops <- getLine
    putStrLn "How many rooks are to be placed on the board?"
    rooks <- getLine
    putStrLn "How many knights are to be placed on the board?"
    knights <- getLine
    putStrLn $ "Hey there  " ++ knights