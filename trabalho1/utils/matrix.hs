module Utils.Matrix (Matrix, Position, createMatrix, getMatrixValue, setMatrixValue, numRows, numCols, matrixToList, listToMatrix, printMatrix) where

import Data.List

data Matrix a = Matrix [[a]] deriving (Eq, Show)
type Position = (Int, Int)

-- Cria uma matriz vazia com as dimensões especificadas
createMatrix :: Int -> Int -> a -> Matrix a
createMatrix rows cols defaultValue = Matrix (replicate rows (replicate cols defaultValue))
-- A função replicate :: Int -> a -> [a] cria uma lista com o número de elementos especificado

-- Obtém o valor em uma posição específica na matriz
getMatrixValue :: Matrix a -> (Position) -> a
getMatrixValue (Matrix rows) (row, col) = (rows !! row) !! col
{-
(Matrix rows) desestrutura o primeiro argumento, extraindo as linhas da matriz. rows agora é do tipo [[a]], ou seja, é uma lista de listas.
(rows !! row) obtém a linha correspondente ao índice row na lista de listas. Isso retorna a linha da matriz que estamos interessados.
-}

-- Define um valor em uma posição específica na matriz
setMatrixValue :: [[a]] -> Position -> a -> [[a]]
setMatrixValue matrix (row, col) value
    | row < 0 || row >= length matrix = error "Invalid row"
    | col < 0 || col >= length (head matrix) = error "Invalid column"
    | otherwise = take row matrix ++ [take col (matrix !! row) ++ [value] ++ drop (col + 1) (matrix !! row)] ++ drop (row + 1) matrix
{-
(Matrix rows) desestrutura o primeiro argumento, extraindo as linhas da matriz. rows agora é do tipo [[a]], ou seja, é uma lista de listas.
take row rows ++ ... ++ drop (row + 1) rows junta as linhas antes da linha que queremos modificar, a linha modificada e as linhas após a linha
que queremos modificar. Isso cria a nova matriz com o valor inserido na posição (row, col).
-}

-- Obtém o número de linhas da matriz
numRows :: Matrix a -> Int
numRows (Matrix rows) = length rows

-- Obtém o número de colunas da matriz
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
printMatrix :: Show a => [[a]] -> IO ()
printMatrix grid = mapM_ (print . map show) grid
