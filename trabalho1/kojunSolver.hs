module KojunSolver where

import Utils.Matrix

-- Gera recursivamente um conjunto com todas as posições da região que contém a posição (row, col)
getRegionFromPosition :: Eq a => [[a]] -> a -> Int -> Int -> [(Int, Int)] -> [(Int, Int)]
getRegionFromPosition regionGrid region row col visited
    | row < 0 || row >= length regionGrid || col < 0 || col >= length (head regionGrid) ||
      regionGrid !! row !! col /= region || (row, col) `elem` visited = visited
    | otherwise =
        let visited' = (row, col) : visited
        in getRegionFromPosition regionGrid region (row - 1) col visited' ++
           getRegionFromPosition regionGrid region (row + 1) col visited' ++
           getRegionFromPosition regionGrid region row (col - 1) visited' ++
           getRegionFromPosition regionGrid region row (col + 1) visited'

-- Verifica se uma posição é válida
isValid :: [[Int]] -> [[Char]] -> Int -> Int -> Int -> Bool
isValid valueGrid regionGrid row col value =
    let region = regionGrid !! row !! col
        regionSize = sum [1 | row <- regionGrid, cell <- row, cell == region]
        neighbors = [(row - 1, col), (row + 1, col), (row, col - 1), (row, col + 1)]
        validNeighbor (r, c) =
            r >= 0 && r < length valueGrid && c >= 0 && c < length (head valueGrid) &&
            valueGrid !! r !! c /= value
    in value >= 1 && value <= regionSize &&
       all validNeighbor neighbors &&
       (row == 0 || valueGrid !! (row - 1) !! col /= value || regionGrid !! (row - 1) !! col /= region || valueGrid !! (row - 1) !! col > value) &&
       (row == length valueGrid - 1 || valueGrid !! (row + 1) !! col /= value || regionGrid !! (row + 1) !! col /= region || valueGrid !! (row + 1) !! col < value)

-- Resolve o quebra-cabeça Kojun
solveKojun :: [[Int]] -> [[Char]] -> [[Int]]
solveKojun valueGrid regionGrid = if solve valueGrid regionGrid 0 0
                                  then valueGrid
                                  else replicate (length valueGrid) (replicate (length (head valueGrid)) 0)
  where
    solve [] _ _ _ = True
    solve (r:rs) regionGrid row col
        | col == length r = solve rs regionGrid (row + 1) 0
        | r !! col /= 0 = solve (r:rs) regionGrid row (col + 1)
        | otherwise =
            let region = regionGrid !! row !! col
            in any (\v -> isValid valueGrid regionGrid row col v &&
                          solve (take row valueGrid ++ [take col r ++ [v] ++ drop (col + 1) r] ++ drop (row + 1) valueGrid) regionGrid row (col + 1))
                   [1..length valueGrid]

-- Função para imprimir a grade
printGrid :: Show a => [[a]] -> IO ()
printGrid grid = mapM_ (print . map show) grid

-- Verifica se a solução é válida
checkSolution :: [[Int]] -> [[Char]] -> [[Int]] -> Bool
checkSolution valueGrid regionGrid solutionGrid =
    let numRows' = length valueGrid
        numCols' = length (head valueGrid)
        isValidCell r c =
            let region = regionGrid !! r !! c
                value = solutionGrid !! r !! c
                regionSize = sum [1 | row <- regionGrid, cell <- row, cell == region]
                neighbors = [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]
                validNeighbor (nr, nc) =
                    nr >= 0 && nr < numRows' && nc >= 0 && nc < numCols' &&
                    solutionGrid !! nr !! nc /= value
            in value >= 1 && value <= regionSize &&
               all validNeighbor neighbors &&
               (r == 0 || regionGrid !! (r - 1) !! c /= region || valueGrid !! (r - 1) !! c > value) &&
               (r == numRows' - 1 || regionGrid !! (r + 1) !! c /= region || valueGrid !! (r + 1) !! c < value)
    in all (\r -> all (\c -> valueGrid !! r !! c == 0 || isValidCell r c) [0..numCols' - 1]) [0..numRows' - 1]