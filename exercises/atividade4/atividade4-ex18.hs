{- Crie uma função que receba três parâmetros Operador, x e y, e retorne o resultado da operação matemática
x Operador y. Os operadores possíveis de informar são +, -, *, /. Leia o Operador, x e y do teclado. -}

calcula :: String -> Double -> Double -> Double
calcula "+" x y = x + y
calcula "-" x y = x - y
calcula "*" x y = x * y
calcula "/" x y = x / y
calcula _ _ _ = 0

main :: IO ()
main = do
    putStrLn "Digite o primeiro número:"
    inputX <- getLine
    let x = read inputX :: Double
    
    putStrLn "Digite o segundo número:"
    inputY <- getLine
    let y = read inputY :: Double

    putStrLn "Digite o operador (+, -, *, /):"
    operador <- getLine

    let resultado = calcula operador x y

    putStrLn ("O resultado da operação é " ++ show resultado ++ ".")
