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
    contents <- readFile "./inputs/8x8/kojun_4.txt"
    
    -- A partir do arquivo, extrai o tamanho da matriz e as matrizes de valores e regiões
    let linesRead = lines contents
    let matrixSize = read (head linesRead) :: Int
    let valueGrid = listToMatrix . map (map read) . take matrixSize . map words . drop 1 $ linesRead
    let regionGrid = listToMatrix . map (map head) . take matrixSize . map words . drop 1 . drop matrixSize $ linesRead
    
    putStrLn "\n> Matriz de valores:"
    printMatrixFormatted valueGrid

    putStrLn "\n> Matriz de regiões:"
    printMatrixFormatted regionGrid

    let maxRegionSize = matrixSize
    -- Valor chute para o tamanho da maior região
    -- Se for menor que o tamanho da matriz, a solução não será encontrada
    -- Se for muito maior que o tamanho da matriz, a solução será encontrada, mas o desempenho será degradado

    -- Resolve o Kojun
    let solution = solveKojun valueGrid regionGrid (0, 0) maxRegionSize

    -- Imprime a solução (ou uma mensagem indicando que não há solução)
    case solution of
        Just solutionGrid -> do
            putStrLn "\n> Solução encontrada:"
            printMatrixFormatted solutionGrid
        Nothing -> print "Não há solução possível."
