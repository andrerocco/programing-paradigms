import Utils.Matrix
import KojunSolver

import Data.Char (isAlpha)

-- Função para filtrar caracteres alfabéticos
filterAlphabetic :: String -> String
filterAlphabetic = filter isAlpha

main :: IO ()
main = do
    -- Lê arquivos que contém
    -- O tamanho N da matriz (N x N) na primeira linha
    -- A matriz de valores (ocupando N linhas)
    -- A matriz de regiões (ocupando N linhas)
    contents <- readFile "./inputs/6x6/kojun_1.txt"
    
    -- lines divide a string em uma lista de strings, cada uma representando uma linha do arquivo
    let linesRead = lines contents
    -- read converte uma string em um valor de um tipo específico
    let matrixSize = read (head linesRead) :: Int
    
    -- Extrai a matriz de valores e em seguida a matriz de regiões
    let valueGrid = map (map read . words) (take matrixSize (drop 1 linesRead)) :: [[Int]]
    let regionGrid = map filterAlphabetic (take matrixSize (drop (matrixSize + 1) linesRead))

    -- Imprime a matriz de valores
    printGrid valueGrid

    -- Imprime a matriz de regiões
    printGrid regionGrid
