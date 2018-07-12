module Main where

import Data.Set

data PieceType = Rook | Bishop | Knight | Queen | King deriving(Show, Eq, Ord)
data ChessPiece = Piece {row::Int, col::Int, piece::PieceType} deriving(Eq, Ord)
data Board = Board{m:: Int, n:: Int, usedPieces:: Set ChessPiece , numberOfPieces:: Int} deriving(Eq, Ord)
class Piece piece where
    attacks :: piece -> ChessPiece -> Bool

class ChessBoard board where
    isSafe :: board -> ChessPiece->Bool
    place :: board -> ChessPiece->Board
    done::board->Bool

kingMoves::Int->Int->[(Int,Int)]
kingMoves r c = [ (r+x,c+y) | x <- [-1,1,0], y <-[-1,1,0]]

knightMoves::Int->Int->[(Int,Int)]
knightMoves r c = [ (r+x,c+y) | x <- [-2,-1,1,2], y <-[-2,-1,1,2], abs x /= abs y]

instance ChessBoard Board where
    isSafe Board{usedPieces=up} c = all (\p -> not(attacks c p) && not(attacks p c)) up
    place Board{m = mm, n = nn, usedPieces=up, numberOfPieces = np} p = Board{m=mm, n=nn, usedPieces=(insert p up), numberOfPieces=np}
    done Board{usedPieces = up, numberOfPieces=n} = length up == n

instance Show ChessPiece where
    show Piece{piece = Rook} = "R"
    show Piece{piece = Bishop} = "B"
    show Piece{piece = Knight} = "N"
    show Piece{piece = Queen} = "Q"
    show Piece{piece = King} = "K"

instance Piece ChessPiece where
    attacks Piece{row = aRow, col = aCol, piece = Rook} Piece{row = r, col = c} = aRow == r || aCol == c
    attacks Piece{row = aRow, col = aCol, piece = Bishop} Piece{row = r, col = c} = abs(r - aRow) == abs(c - aCol)
    attacks Piece{row = aRow, col = aCol, piece = Knight} Piece{row = r, col = c} = (r,c) `elem` (knightMoves aRow aCol)
    attacks Piece{row = aRow, col = aCol, piece = Queen} Piece{row = r, col = c} = r == aRow || c == aCol || abs(r - aRow) == abs(c - aCol)
    attacks Piece{row = aRow, col = aCol, piece = King} Piece{row = r, col = c} = (r,c) `elem` (kingMoves aRow aCol)

createPiece::PieceType->Int->Int->ChessPiece
createPiece p r c = Piece{row=r, col=c, piece=p}

findCandidate::ChessPiece->Board->Set Board
findCandidate Piece{row=r,col=c,piece=p} Board{m=m,n=n,usedPieces=up} = Data.Set.singleton [bs | rr <- [1..m],cc <- [1..n], let piece=createPiece(p,rr,cc), let bs = place Board{m=m,n=n,usedPieces=up} piece, isSafe bs piece ]
--findCandidate Piece{row=r,col=c,piece=p} board = (do
--    rr <- [1..8]
--    cc <- [1..8]
--    let p=createPiece(p,rr,cc)
--    let b=place board p
--    isSafe b)

solution::Board-> [ChessPiece] -> Set Board -> Set Board
solution board (p:ps) solutions = solution board ps (solutions `union` findCandidate p board)
solution board [] solutions = Data.Set.filter done solutions


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