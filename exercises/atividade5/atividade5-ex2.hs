{- Crie uma função com assinatura media :: [Int] -> Float, a qual recebe uma lista de Int e retorna
a média de todos os elementos da lista. Retorne 0 caso a lista for vazia. Não utilize nenhuma função
pronta do Haskell para realizar esta tarefa. DICA: utilize a função fromIntegral para converter um tipo
inteiro para um tipo compat´ıvel com o operador de divisão /. -}

soma :: [Int] -> Int
soma [] = 0
soma (x:xs) = x + soma xs

comprimento :: [Int] -> Int
comprimento [] = 0
comprimento (x:xs) = 1 + comprimento xs

media :: [Int] -> Float
media [] = 0
media (x:xs) = fromIntegral (soma (x:xs)) / fromIntegral (comprimento (x:xs))

main :: IO ()
main = do
    putStrLn "Digite uma lista de números inteiros:"
    input <- getLine
    let lista = read input :: [Int]
    -- Exemplo de entrada: [1,2,3,4,5]
    
    let result = media lista
    putStrLn ("A média dos elementos da lista é " ++ show result)
