module Solver
    ( PieceArray, PositionArray
    , solve
    ) where
import Data.Array (assocs, indices, (!))
import Data.Maybe (isJust, isNothing)

import Hakoiri (BoardStr, Dir(..), Piece, PieceArray, Pos, PositionArray, getWH)

solve :: PieceArray -> PositionArray -> BoardStr -> [Piece]
solve pieces positions board = adjacentPieces
    where
        spaces = findSpaces board
        adjacentPieces = [pieces ! i | i <- indices positions, any (f (positions ! i) (pieces ! i)) spaces]
        f pos piece space = isJust $ isAdjacent space pos piece

findSpaces :: BoardStr -> [Pos]
findSpaces board = [pos | (pos, size) <- assocs board, isNothing size]

isAdjacent :: Pos -> Pos -> Piece -> Maybe Dir
isAdjacent (sx, sy) (px, py) (_, size)
    | sy + 1 == py && inX  = Just DUp
    | sy == py + h && inX  = Just DDown
    | sx + 1 == px && inY  = Just DLeft
    | sx == px + w && inY  = Just DRight
    | otherwise            = Nothing
    where
        (w, h) = getWH size
        inX = sx >= px && sx < px + w
        inY = sy >= py && sy < py + h

