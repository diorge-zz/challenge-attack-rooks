module Rooks
    ( Piece(..)
    , Board
    , highestRooks
    ) where

import Data.Matrix

data Piece = Pawn
           | Rook
           | Empty

type Board = Matrix Piece

highestRooks :: Board -> Int
highestRooks = undefined
