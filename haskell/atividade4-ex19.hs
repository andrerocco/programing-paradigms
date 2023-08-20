{- A conjetura de Goldbach diz que todo número inteiro par maior que 2 pode ser expressado como a soma de
dois números primos. Embora ela nunca foi provada ser verdadeira, ela funciona para números grandes.
Por exemplo, podemos escrever o número 14 como a soma de 7 e 7, ou 18 como a soma de 5 e 13.
Implemente uma função que receba um número n como parâmetro e retorne um dos números primos que
fazem parte da soma. Ex: retorne 5 ou 13 para o caso do número 18. Leia n do teclado. -}

-- Função para verificar se um número é primo
isPrime :: Integer -> Bool
isPrime n
    | n <= 1    = False
    | otherwise = not $ any (\x -> n `mod` x == 0) [2..floor (sqrt (fromIntegral n))]

-- Função para obter um número primo da soma que satisfaz a Conjetura de Goldbach
goldbachPrime :: Integer -> Integer
goldbachPrime n
    | n <= 2 || n `mod` 2 /= 0 = error "O número deve ser par e maior que 2"
    | otherwise = head [x | x <- [2..n-1], isPrime x && isPrime (n - x)]

-- Função principal para ler o valor e encontrar um número primo da soma de Goldbach
main :: IO ()
main = do
    putStrLn "Digite um número par maior que 2:"
    input <- getLine
    let n = read input :: Integer
    
    let result = goldbachPrime n
    putStrLn ("Um número primo da soma de Goldbach para " ++ show n ++ " é " ++ show result)
