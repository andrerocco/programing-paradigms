module Utils.ReadMatrix where

import System.IO
import Data.List.Split

readMatrixFromFile :: String -> IO [[Int]]
readMatrixFromFile filename = do
    contents <- readFile filename
    let rows = lines contents
        matrix = map (map read . words) rows :: [[Int]]
    return matrix
