-- Crie uma função que receba dois números x e y e retorne x^y. Leia x e y do teclado.

main :: IO ()

main = do
    putStrLn "Digite o valor de x: "
    inputX <- getLine
    putStrLn "Digite o valor de y: "
    inputY <- getLine
    let x = read inputX :: Double
          y = read inputY :: Double
    let result = calcularPotencia x y

    putStrLn ("O resultado de " ++ show x ++ " elevado a " ++ show y ++ " é: " ++ show result)

calcularPotencia :: Double -> Double -> Double
calcularPotencia x y = x ** y
