module Lib where

import Data.Set
import Data.Set.Extra
import Data.List.Split

data PieceType = Rook | Bishop | Knight | Queen | King deriving(Show, Eq, Ord)
data ChessPiece = Piece {row::Int, col::Int, piece::PieceType} deriving(Eq, Ord)
data Board = Board{m:: Int, n:: Int, usedPieces:: Set ChessPiece , numberOfPieces:: Int} deriving(Eq, Ord)
class Piece piece where
    attacks :: piece -> ChessPiece -> Bool

class ChessBoard board where
    isSafe :: board -> ChessPiece->Bool
    place :: board -> ChessPiece->Board
    done::board->Bool

instance ChessBoard Board where
    isSafe b c = Prelude.all (\p -> not(attacks c p) && not(attacks p c)) (usedPieces b)
    place Board{m = mm, n = nn, usedPieces=up, numberOfPieces = np} p = Board{m=mm, n=nn, usedPieces=(insert p up), numberOfPieces=np}
    done Board{usedPieces = up, numberOfPieces=n} = length up == n

instance Piece ChessPiece where
    attacks Piece{row = aRow, col = aCol, piece = Rook} Piece{row = r, col = c} = aRow == r || aCol == c
    attacks Piece{row = aRow, col = aCol, piece = Bishop} Piece{row = r, col = c} = abs(r - aRow) == abs(c - aCol)
    attacks Piece{row = aRow, col = aCol, piece = Knight} Piece{row = r, col = c} = (r,c) `elem` (knightMoves aRow aCol)
    attacks Piece{row = aRow, col = aCol, piece = Queen} Piece{row = r, col = c} = r == aRow || c == aCol || abs(r - aRow) == abs(c - aCol)
    attacks Piece{row = aRow, col = aCol, piece = King} Piece{row = r, col = c} = (r,c) `elem` (kingMoves aRow aCol)

kingMoves :: Int -> Int -> [(Int, Int)]
kingMoves r c = [ (r + x, c + y) | x <- [-1, 1, 0], y <- [-1, 1, 0] ]

knightMoves :: Int -> Int -> [(Int, Int)]
knightMoves r c = [ (r + x, c + y) | x <- [-2, -1, 1, 2] , y <- [-2, -1, 1, 2] , abs x /= abs y ]

findCandidate :: ChessPiece -> Board -> Set Board
findCandidate p b
    = Data.Set.fromList $
        [ newBoard
        | rr <- [1 .. (m b)]
        , cc <- [1 .. (n b)]
        , let pp = createPiece (piece p) rr cc
        , let newBoard = place b pp
        , isSafe b pp]

solution :: Board -> [ChessPiece] -> Set Board -> Set Board
solution board (p : ps) solutions = solution board ps (Data.Set.Extra.flatten (Data.Set.map (findCandidate p) solutions))
solution board [] solutions = Data.Set.filter done solutions

chessPieceList :: Int -> Int -> Int -> Int -> Int -> [ChessPiece]
chessPieceList kings queens bishops rooks knights =
           replicate kings   Piece {row=0,col=0,piece = King}
        ++ replicate queens  Piece {row=0,col=0,piece = Queen}
        ++ replicate bishops Piece {row=0,col=0,piece = Bishop}
        ++ replicate knights Piece {row=0,col=0,piece = Knight}
        ++ replicate rooks   Piece {row=0,col=0,piece = Rook}

makeSolution :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Set Board
makeSolution m n kings queens bishops rooks knights = solution
    Board
        { m              = m
        , n              = n
        , usedPieces     = Data.Set.empty
        , numberOfPieces = kings + queens + bishops + rooks + knights
        }
    (chessPieceList kings queens bishops rooks knights)
    (Data.Set.singleton Board
        { m              = m
        , n              = n
        , usedPieces     = Data.Set.empty
        , numberOfPieces = kings + queens + bishops + rooks + knights
        })

createPiece :: PieceType -> Int -> Int -> ChessPiece
createPiece p r c = Piece {row = r, col = c, piece = p}

instance Show ChessPiece where
    show Piece{piece = Rook} = "R"
    show Piece{piece = Bishop} = "B"
    show Piece{piece = Knight} = "N"
    show Piece{piece = Queen} = "Q"
    show Piece{piece = King} = "K"

instance Show Board where
    show Board {m = mm, n= nn, usedPieces = up, numberOfPieces=np} = Prelude.unlines $ Prelude.map Prelude.unwords  $ chunksOf mm [(printPiece p r c) | p <- (toList up), r<-[1..mm], c<-[1..nn]]    

printPiece:: ChessPiece -> Int -> Int -> String
printPiece p r c = if row p == r && col p == c then show p else "-"     