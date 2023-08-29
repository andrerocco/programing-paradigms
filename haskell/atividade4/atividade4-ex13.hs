{- Crie uma função que receba dois números x e y e retorne o mínimo múltiplo comum (DICA: use a função
do máximo divisor comum já criada). Leia x e y do teclado. -}

mdcEuclides :: Int -> Int -> Int
mdcEuclides x 0 = x
mdcEuclides x y = mdcEuclides y (x `mod` y)

mmc :: Int -> Int -> Int
mmc x y = abs (x * y) `div` (mdcEuclides x y)

main :: IO ()
main = do
    putStrLn "Digite o primeiro número:"
    x <- readLn
    putStrLn "Digite o segundo número:"
    y <- readLn
    let resultado = mmc x y
    putStrLn ("O mínimo múltiplo comum de " ++ show x ++ " e " ++ show y ++ " é: " ++ show resultado)