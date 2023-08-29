{- Crie uma função com assinatura busca :: [Int] -> Int -> Bool, a qual recebe uma lista de Int e um
Int e retorna se o elemento passado como parâmetro encontra-se na lista ou não. Não utilize nenhuma
função pronta do Haskell para realizar esta tarefa. -}

busca :: [Int] -> Int -> Bool
busca [] :: False
busca (x:xs) n = do
    if x == n then
        True
    else
        busca xs n

main :: IO ()
main = do
    putStrLn "Digite uma lista de números inteiros:"
    input <- getLine
    let lista = read input :: [Int]
    -- Exemplo de entrada: [1,2,3,4,5]
    
    putStrLn "Digite um número inteiro:"
    input <- getLine
    let n = read input :: Int
    -- Exemplo de entrada: 3
    
    let result = busca lista n
    putStrLn ("O elemento " ++ show n ++ " está na lista? " ++ show result)
