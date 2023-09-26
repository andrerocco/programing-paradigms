module Utils.Matrix
    (Matrix, Position,
createMatrix, getMatrixValue, setMatrixValue, numRows, numCols, matrixToList, listToMatrix, printMatrix, printMatrixFormatted) where

import Data.List

data Matrix a = Matrix [[a]] deriving (Eq, Show)
type Position = (Int, Int)

-- Inicializa uma matriz com um valor padrão
createMatrix :: Int -> Int -> a -> Matrix a
createMatrix rows cols defaultValue = Matrix (replicate rows (replicate cols defaultValue))

-- Obtém o valor em uma posição específica na matriz
getMatrixValue :: Matrix a -> Position -> a
getMatrixValue (Matrix rows) (row, col) = (rows !! row) !! col

-- Define um valor em uma posição específica na matriz
setMatrixValue :: Matrix a -> Position -> a -> Matrix a
setMatrixValue (Matrix rows) (row, col) value
    | row < 0 || row >= length rows = error "Invalid row"
    | col < 0 || col >= length (head rows) = error "Invalid column"
    | otherwise = Matrix (take row rows ++ [take col (rows !! row) ++ [value] ++ drop (col + 1) (rows !! row)] ++ drop (row + 1) rows)

-- Obtém o número de linhas de uma matriz
numRows :: Matrix a -> Int
numRows (Matrix rows) = length rows

-- Obtém o número de colunas de uma matriz
numCols :: Matrix a -> Int
numCols (Matrix []) = 0
numCols (Matrix (row:_)) = length row

-- Converte uma matriz em uma lista de listas
matrixToList :: Matrix a -> [[a]]
matrixToList (Matrix rows) = rows

-- Converte uma lista de listas em uma matriz
listToMatrix :: [[a]] -> Matrix a
listToMatrix = Matrix

-- Função para imprimir a matriz
printMatrix :: Show a => Matrix a -> IO ()
printMatrix (Matrix grid) = mapM_ (print . map show) grid

-- Função para imprimir a matriz de forma formatada
printMatrixFormatted :: Show a => Matrix a -> IO ()
printMatrixFormatted (Matrix grid) = mapM_ (putStrLn . intercalate " ") (map (map show) grid)
