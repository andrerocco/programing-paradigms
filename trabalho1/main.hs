import Utils.Matrix
import KojunSolver

import Data.Char (isAlpha)
import Control.Monad (forM_)

-- Função para filtrar caracteres alfabéticos
filterAlphabetic :: String -> String
filterAlphabetic = filter isAlpha

main :: IO ()
main = do
    -- Lê arquivos que contém
    -- O tamanho N da matriz (N x N) na primeira linha
    -- A matriz de valores (ocupando N linhas)
    -- A matriz de regiões (ocupando N linhas)
    contents <- readFile "./inputs/10x10/kojun_10.txt"
    
    -- lines divide a string em uma lista de strings, cada uma representando uma linha do arquivo
    let linesRead = lines contents
    -- read converte uma string em um valor de um tipo específico
    let matrixSize = read (head linesRead) :: Int
    
    -- Extrai a matriz de valores e a matriz de regiões
    let valueGrid = map (map read . words) (take matrixSize (drop 1 linesRead)) :: [[Int]]
    let regionGrid = map filterAlphabetic (take matrixSize (drop (matrixSize + 1) linesRead))
    {-
    drop 1 linesRead remove a primeira linha (que é o tamanho da matriz) e retorna as linhas restantes.
    take matrixSize pega as primeiras matrixSize linhas após a remoção da primeira linha.
    words transforma uma string em uma lista de palavras (divide a string em uma lista de substrings separadas por espaços). 
    Usar . entre map read e words cria uma nova função que, quando aplicada a uma string, primeiro aplica words para dividir
    a string em palavras e, em seguida, aplica map read a cada palavra resultante, convertendo-as em valores.
    -}
    
    -- Resolve o Kojun
    let solution = solveKojun valueGrid regionGrid 0 0

    -- Imprime a solução (ou uma mensagem indicando que não há solução)
    case solution of
        Just solutionGrid -> printGridFormatted solutionGrid
        Nothing -> print "Não há solução possível."
