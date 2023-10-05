module KojunSolver where

import Utils.Matrix

{- 
A partir de uma matriz de regiões, uma região e uma posição, obtém recursivamente todas as posições da região que contém a
posição passada como parâmetro.
-}
getRegionFromPosition :: Eq a => Matrix a -> a -> Int -> Int -> [(Int, Int)] -> [(Int, Int)]
getRegionFromPosition regionGrid region row col visited
    | row < 0 || row >= numRows regionGrid ||
      col < 0 || col >= numCols regionGrid ||
      getMatrixValue regionGrid (row, col) /= region ||
      (row, col) `elem` visited = visited
    | otherwise =
        let visited' = (row, col) : visited
        in foldr
            (\(r, c) accVisited -> getRegionFromPosition regionGrid region r c accVisited)
            visited' 
            [(row - 1, col), (row + 1, col), (row, col - 1), (row, col + 1)]

{-
Recebe uma matriz de valores (Int), uma matriz de regiões (Char), uma Position e um valor Int e verifica se o valor pode
ser posicionado em uma posição Position da matriz de valores seguindo as regras do Kojun:
1. Inserir um número em cada célula da grade para que cada região de tamanho N contenha cada número de 1 a N exatamente
uma vez.
2. Os números em células ortogonalmente adjacentes devem ser diferentes.
3. Se duas células são adjacentes verticalmente na mesma região, o número na célula superior deve ser maior que o número
na célula inferior.
-}
isValidPlacement :: Matrix Int -> Matrix Char -> Position -> Int -> Bool
isValidPlacement valueGrid regionGrid (row, col) value
        | getMatrixValue valueGrid (row, col) /= 0 = False  -- Verifica se a posição já está ocupada por algum valor diferente de 0
        | value < 1 || value > regionSize = False           -- Verifica se o número está entre 1 e N, onde N é o tamanho da região
        | hasAdjacentValue = False                          -- Verifica se algum dos valores adjacentes é igual ao valor que queremos inserir
        | hasValueInRegion = False                          -- Verifica se o valor já está na região
        | isTopInvalid = False                              -- Se a posição acima for inválida (for da mesma região e menor em valor), retorna False
        | isBottomInvalid = False                           -- Se a posição abaixo for inválida (for da mesma região e maior em valor), retorna False
        | otherwise = True
    where
        numRows' = numRows valueGrid
        numCols' = numCols valueGrid
        region = getMatrixValue regionGrid (row, col)
        
        -- Filtra as posições adjacentes que são válidas (dentro da matriz) e obtém os valores das posições adjacentes
        adjacentPositions =
            filter (\(r, c) -> r >= 0 && r < numRows' && c >= 0 && c < numCols')
            [(row - 1, col), (row + 1, col), (row, col - 1), (row, col + 1)]
        adjacentValues = map (\(r, c) -> getMatrixValue valueGrid (r, c)) adjacentPositions
        -- Obtém as posições da região que contém a posição (row, col)
        regionPositions = getRegionFromPosition regionGrid region row col []
        regionSize = length regionPositions

        -- (1) Verifica se o valor já está presente na região que contém a posição
        hasValueInRegion = value `elem` map (\(r, c) -> getMatrixValue valueGrid (r, c)) regionPositions
        -- (2) Verifica se algum dos valores adjacentes é igual ao valor que queremos inserir
        hasAdjacentValue = any (== value) adjacentValues
        -- (3) Verifica se a posição acima de (row, col) é da mesma região e se o valor é maior
        isTopSameRegion = (row - 1, col) `elem` regionPositions
        isTopInvalid = isTopSameRegion && getMatrixValue valueGrid (row - 1, col) <= value
        -- (3) Verifica se a posição abaixo de (row, col) é da mesma região e se o valor é menor
        isBottomSameRegion = (row + 1, col) `elem` regionPositions
        isBottomInvalid = isBottomSameRegion && getMatrixValue valueGrid (row + 1, col) >= value

{-
Resolve o Kojun a partir de uma matriz de valores (Int) e uma matriz de regiões (Char).
Por ser uma função recursiva, recebe também um ponto de partida (linha e coluna) para aplicar o backtracking. A função
normalmente é chamada externamente com (0, 0).
O retorno é um monad Maybe, que pode ser Nothing (caso não haja solução) ou Just Matrix Int (caso haja solução) onde a
matriz de inteiros representa a solução que foi encontrada para o Kojun.
-}
solveKojun :: Matrix Int -> Matrix Char -> Position -> Int -> Maybe (Matrix Int)
solveKojun valueGrid regionGrid (row, col) maxRegionSize
        | row == numRows valueGrid = Just valueGrid -- Caso chegou ao final da matriz
        | col == numCols valueGrid = solveKojun valueGrid regionGrid (row + 1, 0) maxRegionSize -- Caso chegou ao final da linha, pula para a próxima 
        | getMatrixValue valueGrid (row, col) /= 0 = solveKojun valueGrid regionGrid (row, col + 1) maxRegionSize -- Caso a posição já esteja ocupada, pula para a próxima
        | otherwise = tryValues valueGrid regionGrid row col [1..maxRegionSize] -- Caso nenhuma das condições acima seja satisfeita, chama tryValues com os valores de 1 a N,
    where
        {-
        Tenta colocar valores válidos na posição atual.
        Input: Recebe a matriz de valores, a matriz de regiões, a linha e coluna atual e uma lista de valores a serem testados.
        Output:
        Caso nenhum dos valores da lista seja válido (conferido por isValidPlacement), retorna Nothing.
        Caso algum valor da lista seja válido, chama recursivamente solveKojun com a matriz de valores atualizada.
        -}
        tryValues :: Matrix Int -> Matrix Char -> Int -> Int -> [Int] -> Maybe (Matrix Int)
        tryValues _ _ _ _ [] = Nothing -- Caso não há mais valores para testar, retorna Nothing
        tryValues vg rg r c (value:rest) -- Desconstrói a lista de valores em head (value) e tail (rest)
            | isValidPlacement vg rg (r, c) value =
                case solveKojun (setMatrixValue vg (r, c) value) rg (r, c + 1) maxRegionSize of -- Tenta resolver o Kojun com o valor inserido na posição atual
                    Just result -> Just result
                    Nothing -> tryValues vg rg r c rest -- Nova chamada de tryValue com o restante da lista
            | otherwise = tryValues vg rg r c rest
