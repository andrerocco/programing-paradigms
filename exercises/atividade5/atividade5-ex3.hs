{- Crie uma função com assinatura menor :: [Int] -> Int, a qual recebe uma lista de Int e retorna o
menor elemento da lista. Retorne 0 caso a lista for vazia. Não utilize nenhuma função pronta do Haskell
para realizar esta tarefa. -}

menor :: [Int] -> Int
menor [] = 0
menor (x:xs) = if x < menor xs then x else menor xs

main :: IO ()
main = do
    putStrLn "Digite uma lista de números inteiros:"
    input <- getLine
    let lista = read input :: [Int]
    -- Exemplo de entrada: [1,2,3,4,5]
    
    let result = menor lista
    putStrLn ("O menor elemento da lista é " ++ show result)
