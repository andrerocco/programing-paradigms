{- Crie uma função que compute o n-ésimo número de Fibonacci. Leia n do teclado. -}

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci(n-1) + fibonacci(n-2)

main = do
    putStrLn "Digite um numero: "
    n <- getLine
    let num = (read n :: Int)
    print(fibonacci num)
