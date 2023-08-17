{- Crie uma função que dados dois pontos no espaço 3D, (x1, y1, z1) e (x2, y2, z2), compute a distância
entre eles. Leia as posições dos pontos do teclado. -}

distanciaEntrePontos :: Double -> Double -> Double -> Double -> Double -> Double -> Double
distanciaEntrePontos x1 y1 z1 x2 y2 z2 =
    sqrt ((x2 - x1) ** 2 + (y2 - y1) ** 2 + (z2 - z1) ** 2)

main :: IO ()
main = do
    putStrLn "Digite as coordenadas do primeiro ponto (x1 y1 z1):"
    input1 <- getLine
    -- words input1 divide a string em palavras, usando os espaços como delimitadores
    -- !! i é o operador de índice em Haskell, onde i é um inteiro
    let x1 = read (words input1 !! 0) :: Double
    let y1 = read (words input1 !! 1) :: Double
    let z1 = read (words input1 !! 2) :: Double
    putStrLn "Digite as coordenadas do segundo ponto (x2 y2 z2):"
    input2 <- getLine
    let x2 = read (words input2 !! 0) :: Double
    let y2 = read (words input2 !! 1) :: Double
    let z2 = read (words input2 !! 2) :: Double
        
    let result = distanciaEntrePontos x1 y1 z1 x2 y2 z2
    putStrLn $ "A distância entre os pontos é: " ++ show result
