{- Crie uma função com assinatura mapear :: (t -> y) -> [t] -> [y], a qual recebe uma função, uma
lista e retorna uma lista. Esta função mapear fará o mesmo que a função map, ou seja, aplicar a função
recebida como parâmetro sobre cada elemento da lista e retornar a lista resultante. Não utilize map ou
filter para esta tarefa. -}

mapear :: (t -> y) -> [t] -> [y]
mapear _ [] = []
mapear f (x:xs) = f x : mapear f xs

main :: IO ()
main = do
    putStrLn "Digite uma lista de qualquer tipo:"
    input <- getLine
    let lista = read input :: [Int]
    -- Exemplo de entrada: [1,2,3,4,5]
    
    let result = mapear (*2) lista
    putStrLn ("A lista resultante é " ++ show result)
