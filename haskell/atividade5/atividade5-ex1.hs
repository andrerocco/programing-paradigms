{- Crie uma função com assinatura soma :: [Int] -> Int, a qual recebe uma lista de Int e retorna a soma
de todos os elementos da lista. Retorne 0 caso a lista for vazia. Não utilize nenhuma função pronta do
Haskell para realizar esta tarefa. -}

soma :: [Int] -> Int
soma [] = 0
soma (x:xs) = x + soma xs

main :: IO ()
main = do
    putStrLn "Digite uma lista de números inteiros:"
    input <- getLine
    let lista = read input :: [Int]
    -- Exemplo de entrada: [1,2,3,4,5]
    
    let result = soma lista
    putStrLn ("A soma dos elementos da lista é " ++ show result)
