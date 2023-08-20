{- Crie uma função que receba dois números x e y e determine se eles são coprimos. Dois números são ditos
coprimos se o máximo divisor comum entre eles é 1. Leia x e y do teclado. -}

mdcEuclides :: Int -> Int -> Int
mdcEuclides x 0 = x
mdcEuclides x y = mdcEuclides y (x `mod` y)

coprimos :: Int -> Int -> Bool
coprimos x y = mdcEuclides x y == 1

main :: IO ()
main = do
    putStrLn "Digite o primeiro número:"
    x <- readLn
    putStrLn "Digite o segundo número:"
    y <- readLn
    let resultado = coprimos x y
    if resultado == True then
        putStrLn ("Os números " ++ show x ++ " e " ++ show y ++ " são coprimos.")
    else
        putStrLn ("Os números " ++ show x ++ " e " ++ show y ++ " não são coprimos.")
