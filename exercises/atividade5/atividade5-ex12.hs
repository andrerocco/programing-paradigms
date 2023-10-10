{- Crie uma função com assinatura apagar :: Int -> [t] -> [t], a qual recebe um número de elementos,
uma lista, e retorna uma lista. Esta função deve remover da lista os n primeiros elementos fornecidos
como parâmetro. Por exemplo, a chamada (apagar 3 [1,2,3,4,5]) deve retornar [4,5]. Não utilize
nenhuma função pronta to Haskell para esta tarefa. -}

apagar :: Int -> [t] -> [t]
apagar _ [] = []
apagar 0 lista = lista
apagar n (x:xs) = apagar (n-1) xs

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
    
    let result = apagar n lista
    putStrLn ("A lista sem os " ++ show n ++ " primeiros elementos é " ++ show result)