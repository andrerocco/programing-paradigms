-- Crie uma função que receba um número n e retorne se o mesmo é primo. Leia n do teclado.

isPrime :: Int -> Bool
isPrime n = if n < 2 then False else null [x | x <- [2..n-1], n `mod` x == 0]
-- Percorre a lista de 2 até n-1 e verifica se n é divisível por algum número x.

main :: IO ()
main = do
    putStrLn "Digite um número inteiro:"
    n <- readLn
    let resultado = isPrime n
    if resultado then
        putStrLn ("O número " ++ show n ++ " é primo.")
    else
        putStrLn ("O número " ++ show n ++ " não é primo.")
