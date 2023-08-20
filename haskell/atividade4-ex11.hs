{- Crie uma função que receba dois números x e y e retorne o máximo divisor comum (DICA: pesquise sobre
o Algoritmo de Euclides). Leia x e y do teclado. -}

mdcEuclides :: Integer -> Integer -> Integer
mdcEuclides x 0 = x
mdcEuclides x y = mdcEuclides y (x `mod` y)

main :: IO ()
main = do
    putStrLn "Digite o primeiro número:"
    inputX <- getLine
    let x = read inputX :: Integer
    
    putStrLn "Digite o segundo número:"
    inputY <- getLine
    let y = read inputY :: Integer
    
    let result = mdcEuclides x y
    putStrLn ("O MDC de " ++ show x ++ " e " ++ show y ++ " é " ++ show result)
