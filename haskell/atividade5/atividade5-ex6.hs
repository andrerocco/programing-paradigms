{- Crie uma função com assinatura ocorrencias :: [Int] -> Int -> Int, a qual recebe uma lista de
Int e um Int e retorna o número de vezes em que o elemento está presente na lista. Não utilize nenhuma
função pronta do Haskell para realizar esta tarefa. -}

ocorrencias :: [Int] -> Int -> Int
ocorrencias [] _ = 0
ocorrencias (x:xs) n = if x == n then 1 + ocorrencias xs n else ocorrencias xs n

main :: IO ()
main = do
    putStrLn "Digite uma lista de números inteiros:"
    input <- getLine
    let lista = read input :: [Int]
    -- Exemplo de entrada: [1,2,3,4,5]
    
    putStrLn "Digite um número inteiro:"
    input <- getLine
    let n = read input :: Int
    -- Exemplo de entrada: 1
    
    let result = ocorrencias lista n
    putStrLn ("O número " ++ show n ++ " aparece " ++ show result ++ " vezes na lista")
