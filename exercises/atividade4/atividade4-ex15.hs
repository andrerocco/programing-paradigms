{- Crie uma função que receba um número n e retorne a função totiente de Euler (φ(n)). A função totiente
de Euler é dada pelo número de inteiros positivos r a partir de 1 e menores que n, ou seja 1 <= r < n,
que são coprimos de n. Por exemplo, se n = 10, então os coprimos de 10 de 1 até 10-1 so {1, 3, 7, 9} e
a função deve retornar φ(n) = 4. Leia n do teclado. -}

mdc :: Int -> Int -> Int
mdc x y = if y == 0 then x else mdc y (x `mod` y)

coprimos :: Int -> Int -> Bool
coprimos x y = mdc x y == 1

totienteEuler :: Int -> Int
totienteEuler n = length [x | x <- [1..n-1], coprimos x n]

main :: IO ()
main = do
    putStrLn "Digite um número inteiro:"
    n <- readLn
    let resultado = totienteEuler n
    putStrLn ("A função totiente de Euler de " ++ show n ++ " é: " ++ show resultado)
