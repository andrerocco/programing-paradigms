-- Crie uma função que receba dois números x e y e retorne se x é divisível por y. Leia x e y do teclado.

divisivel :: Int -> Int -> Bool
divisivel x y = x `mod` y == 0

main :: IO ()
main = do
    putStrLn "Digite o primeiro número:"
    x <- readLn
    putStrLn "Digite o segundo número:"
    y <- readLn
    let resultado = divisivel x y
    if resultado then
        putStrLn ("O número " ++ show x ++ " é divisível por " ++ show y ++ ".")
    else
        putStrLn ("O número " ++ show x ++ " NÃO é divisível por " ++ show y ++ ".")
