{- Crie uma função com assinatura inverte :: [t] -> [t], a qual recebe uma lista como parâmetro e
deve retornar a mesma invertida. Não utilize nenhuma função pronta do Haskell para realizar esta tarefa. -}

inverte :: [t] -> [t]
inverte [] = []
inverte (x:xs) = inverte xs ++ [x]

main :: IO ()
main = do
    putStrLn "Digite uma lista de qualquer tipo:"
    input <- getLine
    let lista = read input :: [Int]
    -- Exemplo de entrada: [1,2,3,4,5]
    
    let result = inverte lista
    putStrLn ("A lista invertida é " ++ show result)
