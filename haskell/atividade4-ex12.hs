{- Crie uma função que receba três números x, y e z e retorne o máximo divisor comum (DICA: apenas
modifique o algoritmo anterior). Leia x, y e z do teclado. -}

-- Função para calcular o MDC usando o Algoritmo de Euclides
mdcEuclidees :: Integer -> Integer -> Integer
mdcEuclidees x 0 = x
mdcEuclidees x y = mdcEuclidees y (x `mod` y)

-- Função para calcular o MDC de três números
mdcEuclidesTriplo :: Integer -> Integer -> Integer -> Integer
mdcEuclidesTriplo x y z = mdcEuclidees (mdcEuclidees x y) z

-- Função principal para ler os valores e calcular o MDC de três números
main :: IO ()
main = do
    putStrLn "Digite o primeiro número:"
    inputX <- getLine
    let x = read inputX :: Integer
    
    putStrLn "Digite o segundo número:"
    inputY <- getLine
    let y = read inputY :: Integer
    
    putStrLn "Digite o terceiro número:"
    inputZ <- getLine
    let z = read inputZ :: Integer
    
    let result = mdcEuclidesTriplo x y z
    putStrLn ("O MDC de " ++ show x ++ ", " ++ show y ++ " e " ++ show z ++ " é " ++ show result)
