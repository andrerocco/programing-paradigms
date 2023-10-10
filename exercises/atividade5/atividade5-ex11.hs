{- Crie uma função com assinatura primeiros :: Int -> [t] -> [t], a qual recebe um número de elementos,
uma lista, e retorna uma lista. Esta função deve retornar uma lista com os n primeiros elementos
informados no primeiro parâmetro. Não utilize nenhuma função pronta to Haskell para esta tarefa. -}

primeiros :: Int -> [t] -> [t]
primeiros _ [] = []
primeiros 0 _ = []
primeiros n (x:xs) = x : primeiros (n-1) xs

main :: IO ()
main = do
    putStrLn "Digite uma lista de qualquer tipo:"
    input <- getLine
    let lista = read input :: [Int]
    -- Exemplo de entrada: [1,2,3,4,5]
    
    putStrLn "Digite um número inteiro:"
    input <- getLine
    let n = read input :: Int
    -- Exemplo de entrada: 3
    
    let result = primeiros n lista
    putStrLn ("Os " ++ show n ++ " primeiros elementos da lista são " ++ show result)
