{-|
Module: Rooks
Description: solution for the Attack Rooks problem

Provides a solution for the Attack Rooks problem (N-Pawn/Rook problem).
-}
module Rooks
    ( Piece(..)
    , Board
    , highestRooks
    ) where

import Data.Matrix
import qualified Data.Vector as Vec

-- | Define a Piece on the board
data Piece = Pawn -- ^ A blocking Pawn
           | Rook -- ^ A placed Rook
           | Empty -- ^ An empty slot
            deriving (Show, Eq)

-- | Defines a Board (synonym to Matrix Piece)
type Board = Matrix Piece

-- | Solves the problem for a given board
highestRooks :: Board -> Int
highestRooks b = rookCount b + highestRooks' b


highestRooks' :: Board -> Int
highestRooks' = (+1) . vecmax . Vec.map highestRooks' . derivedStates
    where vecmax v | Vec.null v = (-1)
                   | otherwise  = Vec.maximum v

rookCount :: Board -> Int
rookCount = Vec.length . Vec.filter (== Rook) . getMatrixAsVector

derivedStates :: Board -> Vec.Vector Board
derivedStates b = Vec.map (placeRook b) . Vec.filter (canPutRook b) $ positions b

positions :: Board -> Vec.Vector (Int, Int)
positions b = Vec.map (\(x,y) -> (x+1, y+1)) $ Vec.generate (n*n)
                (\i -> (i `mod` n, i `div` n))
                where n = nrows b

placeRook :: Board -> (Int, Int) -> Board
placeRook b p = setElem Rook p b

canPutRook :: Board -> (Int, Int) -> Bool
canPutRook b (row, col) = canPutRookInLine (getCol col b) row
                       && canPutRookInLine (getRow row b) col

canPutRookInLine :: Vec.Vector Piece -> Int -> Bool
canPutRookInLine line pos = (Vec.foldr (step pos)
                                       MayPut
                                       (Vec.zip (Vec.generate 3 (+1))  line)
                            ) `elem` [Ok, Put]

data FoldState = MayPut | CannotPut | Put | Ok | Invalid deriving (Show, Eq)

step :: Int -> (Int, Piece) -> FoldState -> FoldState
step pos (i, Pawn) MayPut = if pos == i then Invalid else MayPut
step pos (i, Rook) MayPut = CannotPut
step pos (i, Empty) MayPut = if pos == i then Put else MayPut
step pos (i, Pawn) CannotPut = MayPut
step pos (i, Empty) CannotPut = if pos == i then Invalid else CannotPut
step pos (i, Pawn) Put = Ok
step pos (i, Rook) Put = Invalid
step pos (i, Empty) Put = Put
step pos (i, p) Ok = Ok
step pos (i, p) Invalid = Invalid
step _ _ _ = Invalid -- impossible, but we're making the function total anyway
