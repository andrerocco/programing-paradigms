-- Crie uma função que receba um número x, negativo ou positivo, e retorne seu valor absoluto. Leia x do teclado.

main :: IO ()

main = do
    putStrLn "Digite o valor x: "
    inputX <- getLine
    let x = read inputX :: Double
    
    let result = calcularValorAbsoluto x
    putStrLn("O valor absoluto de " ++ show x ++ " é " ++ show result)

calcularValorAbsoluto :: Double -> Double
calcularValorAbsoluto x = 
    if x < 0 then
        x * (-1)
    else x
