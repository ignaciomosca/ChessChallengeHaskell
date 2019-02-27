module Main where

import Data.Set
import Lib

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
    putStrLn "Display results? Y/n"
    displayResults <- getLine
    putStrLn (showResults displayResults (makeSolution m n kings queens bishops rooks knights))

showResults::String -> Set Board-> String
showResults "Y" boards = unlines $ Prelude.map show $ toList(boards)
showResults _ boards = show $ length $ boards
