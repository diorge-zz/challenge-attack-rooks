module Main
    where

import Rooks
import Data.Matrix
import Data.List.Split (splitOn)
import System.Environment (getArgs)
import Control.Monad (forM_)

main :: IO ()
main = getArgs >>= flip forM_ process

process :: String -> IO ()
process = printResult . calculate

printResult :: Maybe Int -> IO ()
printResult Nothing  = putStrLn "Invalid input"
printResult (Just r) = print r

calculate :: String -> Maybe Int
calculate = fmap highestRooks . validate valid . parse

validate :: (a -> Bool) -> Maybe a -> Maybe a
validate _ Nothing = Nothing
validate f (Just x) = check (f x) x
        where   check True  a = Just a
                check False _ = Nothing

valid :: Board -> Bool
valid b = ncols b == nrows b

parse :: String -> Maybe Board
parse = fmap fromLists . parse'

parse' :: String -> Maybe [[Piece]]
parse' = allJust . map parseRow . splitOn "|"

allJust :: [Maybe [a]] -> Maybe [[a]]
allJust [] = Just []
allJust ((Just []):_) = Just []
allJust (Nothing:_) = Nothing
allJust ((Just x):xs) = fmap (x:) (allJust xs)

parseRow :: String -> Maybe [Piece]
parseRow []       = Just []
parseRow ('.':xs) = fmap (Empty :) (parseRow xs)
parseRow ('X':xs) = fmap (Pawn  :) (parseRow xs)
parseRow _        = Nothing
