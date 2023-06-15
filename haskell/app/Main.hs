module Main (main) where

import Hakoiri (parseBoard)

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
    print $ parseBoard initialArrange
