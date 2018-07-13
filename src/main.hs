module Main where

import Data.Set
import Data.Set.Extra

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
    isSafe Board{usedPieces=up} c = Prelude.all (\p -> not(attacks c p) && not(attacks p c)) up
    place Board{m = mm, n = nn, usedPieces=up, numberOfPieces = np} p = Board{m=mm, n=nn, usedPieces=(insert p up), numberOfPieces=np}
    done Board{usedPieces = up, numberOfPieces=n} = length up == n

instance Piece ChessPiece where
    attacks Piece{row = aRow, col = aCol, piece = Rook} Piece{row = r, col = c} = aRow == r || aCol == c
    attacks Piece{row = aRow, col = aCol, piece = Bishop} Piece{row = r, col = c} = abs(r - aRow) == abs(c - aCol)
    attacks Piece{row = aRow, col = aCol, piece = Knight} Piece{row = r, col = c} = (r,c) `elem` (knightMoves aRow aCol)
    attacks Piece{row = aRow, col = aCol, piece = Queen} Piece{row = r, col = c} = r == aRow || c == aCol || abs(r - aRow) == abs(c - aCol)
    attacks Piece{row = aRow, col = aCol, piece = King} Piece{row = r, col = c} = (r,c) `elem` (kingMoves aRow aCol)

findCandidate :: ChessPiece -> Board -> Set Board
findCandidate Piece { row = r, col = c, piece = p } Board { m = m, n = n, usedPieces = up, numberOfPieces = nps }
    = Data.Set.fromList
        [ bs
        | rr <- [1 .. m]
        , cc <- [1 .. n]
        , let pp = createPiece p rr cc
        , let
            bs = place
                Board {m = m, n = n, usedPieces = up, numberOfPieces = nps}
                pp
        , isSafe bs pp
        ]

solution :: Board -> [ChessPiece] -> Set Board -> Set Board
solution board (p : ps) solutions = solution board ps (Data.Set.Extra.flatten (Data.Set.map (findCandidate p) solutions))
solution board [] solutions = Data.Set.filter done solutions

chessPieceList :: Int -> Int -> Int -> Int -> Int -> [ChessPiece]
chessPieceList kings queens bishops rooks knights =
    replicate kings Piece {piece = King}
        ++ replicate queens  Piece {piece = Queen}
        ++ replicate bishops Piece {piece = Bishop}
        ++ replicate knights Piece {piece = Knight}
        ++ replicate rooks   Piece {piece = Rook}

makeSolution :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Set Board
makeSolution m n kings queens bishops rooks knights = solution
    Board
        { m              = m
        , n              = n
        , usedPieces     = Data.Set.empty
        , numberOfPieces = kings + queens + bishops + rooks + knights
        }
    (chessPieceList kings queens bishops rooks knights)
    Data.Set.empty

main = do
    putStrLn "Enter M"
    m <- readLn :: IO Int
    putStrLn "Enter N"
    n <- readLn :: IO Int
    putStrLn "How many kings are to be placed on the board?"
    kings <- readLn :: IO Int
    putStrLn "How many queens are to be placed on the board?"
    queens <- readLn :: IO Int
    putStrLn "How many bishops are to be placed on the board?"
    bishops <- readLn :: IO Int
    putStrLn "How many rooks are to be placed on the board?"
    rooks <- readLn :: IO Int
    putStrLn "How many knights are to be placed on the board?"
    knights <- readLn :: IO Int
    print (length $ makeSolution m n kings queens bishops rooks knights)

kingMoves :: Int -> Int -> [(Int, Int)]
kingMoves r c = [ (r + x, c + y) | x <- [-1, 1, 0], y <- [-1, 1, 0] ]

knightMoves :: Int -> Int -> [(Int, Int)]
knightMoves r c = [ (r + x, c + y) | x <- [-2, -1, 1, 2] , y <- [-2, -1, 1, 2] , abs x /= abs y ]

createPiece :: PieceType -> Int -> Int -> ChessPiece
createPiece p r c = Piece {row = r, col = c, piece = p}

setfmap :: Board -> ChessPiece -> Set Board -> Set Board -> Set Board
setfmap p b b1 b2 = Data.Set.empty

instance Show ChessPiece where
    show Piece{piece = Rook} = "R"
    show Piece{piece = Bishop} = "B"
    show Piece{piece = Knight} = "N"
    show Piece{piece = Queen} = "Q"
    show Piece{piece = King} = "K"