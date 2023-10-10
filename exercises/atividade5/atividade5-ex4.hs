{- Crie uma função com assinatura diferencaMaiorMenor :: [Int] -> Int, a qual recebe uma lista de
Int e retorna a diferen¸ca entre o maior e o menor elemento da lista. Retorne 0 caso a lista for vazia. Não
utilize nenhuma função pronta do Haskell para realizar esta tarefa. -}

maior :: [Int] -> Int
maior [] = 0
maior (x:xs) = if x > maior xs then x else maior xs

menor :: [Int] -> Int
menor [] = 0
menor (x:xs) = if x < menor xs then x else menor xs

diferencaMaiorMenor :: [Int] -> Int
diferencaMaiorMenor [] = 0
diferencaMaiorMenor (x:xs) = maior (x:xs) - menor (x:xs)

main :: IO ()
main = do
    putStrLn "Digite uma lista de números inteiros:"
    input <- getLine
    let lista = read input :: [Int]
    -- Exemplo de entrada: [1,2,3,4,5]
    
    let result = diferencaMaiorMenor lista
    putStrLn ("A diferença entre o maior e o menor elemento da lista é " ++ show result)
