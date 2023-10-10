{- Crie uma função que receba 3 valores numéricos (a, b, c) e retorne o maior deles. Não utilize nenhuma
forma de ordenação. Leia os valores a, b, c do teclado. -}

maiorDeTres :: Double -> Double -> Double -> Double
maiorDeTres a b c
    | a > b && a > c = a
    | b > a && b > c = b
    | otherwise = c

main :: IO ()
main = do
    putStrLn "Digite os três valores (a b c):"
    input <- getLine
    let a = read (words input !! 0) :: Double
    let b = read (words input !! 1) :: Double
    let c = read (words input !! 2) :: Double

    let result = maiorDeTres a b c

    putStrLn $ "O maior valor é: " ++ show result
