module Solver
    ( Hand, PieceArray, PositionArray
    , solve
    ) where
import Data.Array (assocs, indices, (!))
import Data.Maybe (catMaybes, isNothing)

import Hakoiri ( BoardStr, Dir(..), Piece, PieceArray, Pos, PositionArray
               , boardW, boardH, getWH )

type Hand = (Int, Dir)

solve :: PieceArray -> PositionArray -> BoardStr -> [Hand]
solve = findMovablePieces

findMovablePieces :: PieceArray -> PositionArray -> BoardStr -> [Hand]
findMovablePieces pieces positions board = adjacentPieces
    where
        adjacentPieces = catMaybes [(,) i <$> f (positions ! i) (pieces ! i) space | i <- indices positions, space <- spaces]
        spaces = findSpaces board
        f pos piece space = do
            dir <- isAdjacent space pos piece
            if isMovable pos piece dir board
                then Just dir
                else Nothing

findSpaces :: BoardStr -> [Pos]
findSpaces board = [pos | (pos, size) <- assocs board, isNothing size]

isMovable :: Pos -> Piece -> Dir -> BoardStr -> Bool
isMovable (x, y) (_, size) dir board
    | isHorz     = not isWallX && all (\dy -> isNothing (board ! (ax, y + dy))) [0..h-1]
    | otherwise  = not isWallY && all (\dx -> isNothing (board ! (x + dx, ay))) [0..w-1]
    where
        (w, h) = getWH size
        isHorz = dir == DLeft || dir == DRight
        isWallX = (dir == DLeft && x <= 0) || (dir == DRight && x + w >= boardW)
        ax = if dir == DLeft then x - 1 else x + w
        isWallY = (dir == DUp && y <= 0) || (dir == DDown && y + h >= boardH)
        ay = if dir == DUp then y - 1 else y + h

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

