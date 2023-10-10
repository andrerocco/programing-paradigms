-- Crie uma função que receba a base e a altura de um triângulo e calcule a área do mesmo. Leia a base e a altura do teclado.

main :: IO ()
main = do
    putStrLn "Digite a base do triângulo:"
    inputBase <- getLine
    putStrLn "Digite a altura do triângulo:"
    inputAltura <- getLine

    let base = read inputBase :: Double
        altura = read inputAltura :: Double

    let area = calcularAreaTriangulo base altura

    putStrLn ("A área do triângulo com base " ++ show base ++ " e altura " ++ show altura ++ " é: " ++ show area)

calcularAreaTriangulo :: Double -> Double -> Double
calcularAreaTriangulo base altura = (base * altura) / 2