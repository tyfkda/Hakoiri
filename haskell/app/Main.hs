module Main (main) where

import Control.Monad (forM_)
import Data.Array (indices, (!))
import Data.Array.ST (MArray (newArray), runSTArray, writeArray)

import Hakoiri (boardH, boardW, getWH, parseBoard)
import Solver (PieceArray, PositionArray, solve)

initialArrange :: [String]
initialArrange = [
    "1002",
    "1002",
    "3554",
    "3784",
    "6..9"
    ]

printBoard :: PositionArray -> PieceArray -> IO ()
printBoard positions pieces = do
    forM_ [0..boardH-1] $ \y -> do
        forM_ [0..boardW-1] $ \x -> do
            let c = boardArray ! (x, y)
            let cc = if c < 0 then "." else show c
            putStr cc
        putStrLn ""
    where
        boardArray = runSTArray $ do
            arr <- newArray ((0, 0), (boardW - 1, boardH - 1)) (-1)
            forM_ (indices positions) $ \i -> do
                let pos = positions ! i
                let (c, size) = pieces ! i
                let (w, h) = getWH size
                forM_ [(dx, dy) | dy <- [0..h-1], dx <- [0..w-1]] $ \dxy -> do
                    writeArray arr (pos + dxy) c
            return arr

main :: IO ()
main = do
    let (board, positions, pieces) = parseBoard initialArrange
    case solve pieces positions board of
        Just (aa, pp) -> do
            putStrLn $ show (length aa) ++ ": " ++ show (reverse aa)
            printBoard pp pieces
        Nothing -> do
            putStrLn "No solution"
