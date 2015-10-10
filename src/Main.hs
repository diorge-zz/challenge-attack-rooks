module Main
    where

import Rooks
import Data.Matrix
import Data.List.Split (splitOn)

main :: IO ()
main = undefined

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
