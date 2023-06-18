{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hakoiri
    ( BitBoard, BoardStr, Dir(..), Piece, PieceArray, Pos, PositionArray, Size(..)
    , boardH, boardW, getDXY, getWH, parseBoard
    ) where

import Data.Array (Array)
import Data.Array.Base (array)
import Data.Array.ST (runSTArray, MArray (..), writeArray)
import Data.Bits (shiftL, (.&.), (.|.))
import Data.Char (isDigit, ord)
import Data.Int (Int64)
import Data.Hashable (Hashable (..))
import Data.List (foldl', unfoldr)
import Control.Monad (forM_)

type BoardStr = Array Pos (Maybe Size)
type Pos = (Int, Int)
type Piece = (Int, Size)
type PositionArray = Array Int Pos
type PieceArray = Array Int Piece
type BitBoard = Int64

instance (Num a, Num b) => Num (a, b) where
    (x1, y1) + (x2, y2) = (x1 + x2, y1 + y2)
    (x1, y1) - (x2, y2) = (x1 - x2, y1 - y2)
    _ * _ = undefined
    negate (x, y) = (-x, -y)
    abs _ = undefined
    signum _ = undefined
    fromInteger x = (fromInteger x, 0)

boardW :: Int
boardW = 4

boardH :: Int
boardH = 5

space :: Char
space = '.'

hex2Deci :: Char -> Maybe Int
hex2Deci c
    | isDigit c             = Just $ ord c - ord '0'
    | 'A' <= c && c <= 'F'  = Just $ ord c - ord 'A' + 10
    | 'a' <= c && c <= 'f'  = Just $ ord c - ord 'a' + 10
    | otherwise             = Nothing

data Size = Size1x1 | Size2x1 | Size1x2 | Size2x2
    deriving (Eq, Show)

instance Hashable Size where
    hashWithSalt i Size1x1 = i * 977 + 1
    hashWithSalt i Size2x1 = i * 977 + 2
    hashWithSalt i Size1x2 = i * 977 + 3
    hashWithSalt i Size2x2 = i * 977 + 4

getSize :: Int -> Int -> Maybe Size
getSize w h
    | w == 1 && h == 1  = Just Size1x1
    | w == 2 && h == 1  = Just Size2x1
    | w == 1 && h == 2  = Just Size1x2
    | w == 2 && h == 2  = Just Size2x2
    | otherwise         = Nothing

getWH :: Size -> Pos
getWH size = case size of
    Size1x1  -> (1, 1)
    Size2x1  -> (2, 1)
    Size1x2  -> (1, 2)
    Size2x2  -> (2, 2)

data Dir = DLeft | DRight | DUp | DDown
    deriving (Eq, Show)

getDXY :: Dir -> Pos
getDXY DLeft  = (-1,  0)
getDXY DRight = ( 1,  0)
getDXY DUp    = ( 0, -1)
getDXY DDown  = ( 0,  1)

parsePiece :: Pos -> BitBoard -> [String] -> Maybe (Int, Size, BitBoard)
parsePiece (x, y) bitboard rawboard = result
    where
        result | validRect  = hex2Deci c >>= \jc -> size >>= \js -> Just (jc, js, b')
               | otherwise  = Nothing
        size = getSize w h
        c = rawboard !! y !! x
        w = head $ dropWhile (\i -> x + i < boardW && rawboard !! y !! (x + i) == c) [1..]
        h = head $ dropWhile (\i -> y + i < boardH && rawboard !! (y + i) !! (x + w - 1) == c) [1..]
        validRect = all (\(i, j) -> rawboard !! (y + i) !! (x + j) == c) [(i, j) | i <- [0..h-1], j <- [0..w-1]]
        b' = foldl' f bitboard [(i, j) | i <- [0..h-1], j <- [0..w-1]]
        f bb (i, j) = bb .|. (1 `shiftL` ((y + i) * boardW + (x + j)))

parsePieces :: [String] -> [(Pos, Piece)]
parsePieces rawboard = unfoldr f ((0, 0), 0)
    where
        f ((_, y), _) | y >= boardH  = Nothing
        f (pos, bb) = case parsePiece pos bb rawboard of
            Just (c, size, bb')  -> Just ((pos, (c, size)), (nextPos pos bb', bb'))
            Nothing              -> Nothing  -- TODO: Report error
        nextPos (x, y) bb
            | x >= boardW       = nextPos (0, y + 1) bb
            | y >= boardH       = (x, y)
            | have (x, y) bb  = (x, y)
            | otherwise         = nextPos (x + 1, y) bb
        have (x, y) bb = (bb .&. (1 `shiftL` (y * boardW + x))) == 0 &&
                         (rawboard !! y !! x /= space)

parseBoard :: [String] -> (BoardStr, PositionArray, PieceArray)
parseBoard rawboard = (board, positions, pieces)
    where
        positions = array indices $ map (\(pos, (i, _)) -> (i, pos)) pieceList
        pieces = array indices $ map (\(_, piece@(i, _)) -> (i, piece)) pieceList
        indices = foldl' (\(a, b) (_, (i, _)) -> (min i a, max i b)) (0, 0) pieceList
        pieceList = parsePieces rawboard
        board = runSTArray $ do
            arr <- newArray ((0, 0), (boardW - 1, boardH - 1)) Nothing
            forM_ pieceList $ \((x, y), (_, size)) -> do
                let (w, h) = getWH size
                forM_ [(dx, dy) | dy <- [0..h-1], dx <- [0..w-1]] $ \(dx, dy) -> do
                    writeArray arr (x + dx, y + dy) $ Just size
            return arr
