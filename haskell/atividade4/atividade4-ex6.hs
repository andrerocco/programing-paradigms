{- Crie uma função que receba três inteiros x, y e z e retorne se havendo varetas com esses valores em
comprimento pode-se construir um triângulo. Exemplo, com varetas de comprimento 4, 8 e 9 posso
construir um triângulo, porém com varetas de comprimento 10, 5 e 4 não posso construir um triângulo.
Leia x, y e z do teclado. -}

podeConstruirTriangulo :: Int -> Int -> Int -> Bool
podeConstruirTriangulo x y z = (x + y > z) && (x + z > y) && (y + z > x)

main :: IO ()
main = do
    putStrLn "Digite o comprimento da primeira vareta:"
    input1 <- getLine
    let x = read input1 :: Int
    
    putStrLn "Digite o comprimento da segunda vareta:"
    input2 <- getLine
    let y = read input2 :: Int
    
    putStrLn "Digite o comprimento da terceira vareta:"
    input3 <- getLine
    let z = read input3 :: Int
    
    let result = podeConstruirTriangulo x y z
    if result
        then putStrLn "É possível construir um triângulo com esses comprimentos."
        else putStrLn "Não é possível construir um triângulo com esses comprimentos."
