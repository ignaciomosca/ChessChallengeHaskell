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
    mapM_ putStrLn $ Prelude.map show $ toList(makeSolution m n kings queens bishops rooks knights)