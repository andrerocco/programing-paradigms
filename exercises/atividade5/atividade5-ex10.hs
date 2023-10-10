{- Crie uma função com assinatura filtrar :: (t -> Bool) -> [t] -> [t], a qual recebe uma função,
uma lista e retorna uma nova lista. Esta função aplica a função recebida como parˆametro sobre cada
elemento da lista e caso o retorno da função for verdadeiro, então o elemento fará parte da nova lista, caso
contrário não. Para esta tarefa, utilize o conceito de list comprehension. -}

filtrar :: (t -> Bool) -> [t] -> [t]
filtrar _ [] = []
filtrar f (x:xs) = [x | x <- (x:xs), f x]

main :: IO ()
main = do
    putStrLn "Digite uma lista de qualquer tipo:"
    input <- getLine
    let lista = read input :: [Int]
    -- Exemplo de entrada: [1,2,3,4,5]
    
    let result = filtrar even lista
    putStrLn ("A lista resultante é " ++ show result)

