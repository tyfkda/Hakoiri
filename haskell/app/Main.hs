module Main (main) where

import Hakoiri (parseBoard)
import Solver (solve)

initialArrange :: [String]
initialArrange = [
    "1002",
    "1002",
    "3554",
    "3784",
    "6..9"
    ]

main :: IO ()
main = do
    let (board, positions, pieces) = parseBoard initialArrange
    print $ solve pieces positions board
