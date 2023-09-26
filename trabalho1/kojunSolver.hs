module KojunSolver where

import Utils.Matrix

-- Gera recursivamente uma lista com todas as posições da região que contém a posição (row, col)
getRegionFromPosition :: (Eq a) => [[a]] -> a -> Int -> Int -> [(Int, Int)] -> [(Int, Int)]
getRegionFromPosition regionGrid region row col visited
    | row < 0
        || row >= length regionGrid             -- length regionGrid retorna o número de linhas da matriz
        || col < 0
        || col >= length (head regionGrid)      -- length (head regionGrid) retorna o número de colunas do primeiro elemento da matriz
        || regionGrid !! row !! col /= region   -- regionGrid !! row !! col retorna o elemento da matriz na posição (row, col)
        || (row, col) `elem` visited =          -- Verifica se a posição atual (row, col) já está na lista de posições visitadas
        visited                                 -- Se todas as guardas forem atendidas, retornam visited (a lista de posições visitadas até agora)
    | otherwise =
        let visited' = (row, col) : visited
        in foldr
            (\(r, c) accVisited -> getRegionFromPosition regionGrid region r c accVisited) visited' -- Acumula o resultado das chamadas nas posições adjacentes
            [(row - 1, col), (row + 1, col), (row, col - 1), (row, col + 1)]
{- 
A função getRegionFromPosition é uma função polimórfica em Haskell. Ela recebe os seguintes argumentos:
- Uma matriz representada por uma lista de listas, onde o tipo dos elementos é a.
- Um elemento da matriz (representando a região que queremos explorar).
- Coordenadas (row, col) que indicam a posição inicial na matriz.
- Uma lista de posições visitadas.
A parte (Eq a) => na assinatura da função é uma restrição de classe de tipo, especificando um requisito sobre o tipo a
que a função pode aceitar. Nesse caso, (Eq a) significa que o tipo a deve ser uma instância da classe de tipos Eq, que
é a classe de tipos dos tipos que suportam a operação de igualdade (==).
A lista [(row - 1, col), (row + 1, col), (row, col - 1), (row, col + 1)] não é diretamente parte da função lambda.
É usada dentro da função foldr para mapear sobre as posições adjacentes e chamar getRegionFromPosition para cada uma delas.
-}

-- Verifica se um valor Int pode ser posicionado em uma posição Position da matriz de valores seguindo as regras do Kojun:
-- 1. Inserir um número em cada célula da grade para que cada região de tamanho N contenha cada número de 1 a N exatamente uma vez.
-- 2. Os números em células ortogonalmente adjacentes devem ser diferentes.
-- 3. Se duas células são adjacentes verticalmente na mesma região, o número na célula superior deve ser maior que o número na célula inferior.
isValidPlacement :: [[Int]] -> [[Char]] -> Position -> Int -> Bool
isValidPlacement valueGrid regionGrid (row, col) value
    | valueGrid !! row !! col /= 0 = False      -- Verifica se a posição já está ocupada por algum valor diferente de 0
    | value < 1 || value > regionSize = False   -- Verifica se o número está entre 1 e N, onde N é o tamanho da região
    | hasAdjacentValue = False                  -- Verifica se algum dos valores adjacentes é igual ao valor que queremos inserir
    | hasValueInRegion = False                  -- Verifica se o valor já está na região
    | isTopInvalid = False                      -- Se a posição acima for inválida (for da mesma região e menor em valor), retorna False
    | isBottomInvalid = False                   -- Se a posição abaixo for inválida (for da mesma região e maior em valor), retorna False
    | otherwise = True
  where
    numRows = length valueGrid
    numCols = length (head valueGrid)
    region = regionGrid !! row !! col
    -- Filtra as posições adjacentes que são válidas (dentro da matriz) e obtém os valores das posições adjacentes
    adjacentPositions = filter (\(r, c) -> r >= 0 && r < numRows && c >= 0 && c < numCols) [(row - 1, col), (row + 1, col), (row, col - 1), (row, col + 1)]
    adjacentValues = map (\(r, c) -> valueGrid !! r !! c) adjacentPositions
    -- Obtém as posições da região que contém a posição (row, col)
    regionPositions = getRegionFromPosition regionGrid region row col []
    regionSize = length regionPositions

    -- (2) Verifica se algum dos valores adjacentes é igual ao valor que queremos inserir
    hasAdjacentValue = any (== value) adjacentValues

    -- (1) Verifica se o valor já está presente na região que contém a posição
    hasValueInRegion = value `elem` map (\(r, c) -> valueGrid !! r !! c) regionPositions

    -- (3) Verifica se a posição acima de (row, col) é da mesma região e se o valor é maior
    isTopSameRegion = (row - 1, col) `elem` regionPositions
    isTopInvalid = isTopSameRegion && valueGrid !! (row - 1) !! col <= value

    -- (3) Verifica se a posição abaixo de (row, col) é da mesma região e se o valor é menor
    isBottomSameRegion = (row + 1, col) `elem` regionPositions
    isBottomInvalid = isBottomSameRegion && valueGrid !! (row + 1) !! col >= value   

-- Resolve o Kojun recursivamente
solveKojun :: [[Int]] -> [[Char]] -> Int -> Int -> Maybe [[Int]]
solveKojun valueGrid regionGrid row col
    | row == length valueGrid = Just valueGrid
    | col == length (head valueGrid) = solveKojun valueGrid regionGrid (row + 1) 0
    | valueGrid !! row !! col /= 0 = solveKojun valueGrid regionGrid row (col + 1)
    | otherwise = tryValues valueGrid regionGrid row col [1..length valueGrid]
  where
    tryValues :: [[Int]] -> [[Char]] -> Int -> Int -> [Int] -> Maybe [[Int]]
    tryValues _ _ _ _ [] = Nothing
    tryValues vg rg r c (value:rest)
        | isValidPlacement vg rg (r, c) value = 
            case solveKojun (setMatrixValue vg (r, c) value) rg r (c + 1) of
                Just result -> Just result
                Nothing -> tryValues vg rg r c rest
        | otherwise = tryValues vg rg r c rest
    {- 
    take r vg ++ ...:
        take r vg pega as primeiras r linhas da grade original vg.
    [take c (vg !! r) ++ [value] ++ drop (c + 1) (vg !! r)] ++ ...:
        vg !! r pega a linha r da grade original.
        take c (vg !! r) pega os primeiros c elementos dessa linha, ou seja, as colunas até a coluna c - 1.
        drop (c + 1) (vg !! r) pega os elementos a partir da coluna c + 1 até o final da linha.
        [value] é uma lista contendo apenas o value que queremos inserir e será concatenada com as colunas até a coluna c - 1.
    ... ++ drop (r + 1) vg:
        drop (r + 1) vg pega as linhas a partir da linha r + 1 até o final da grade original.
     -}

-- Função que imprime uma grade de forma formatada
printGridFormatted :: Show a => [[a]] -> IO ()
printGridFormatted grid =
    mapM_ (putStrLn . unwords . map show) grid
