{- Crie uma função que receba dois valores booleanos (x, y) retorne o resultado do “ou exclusivo” (XOR)
sobre eles. A função apenas deve usar os operadores &&, || e not. Leia os valores x e y do teclado. -}

xor :: Bool -> Bool -> Bool
xor x y = (x || y) && not (x && y)

main :: IO ()
main = do
    putStrLn "Digite o primeiro valor booleano (True ou False):"
    input1 <- getLine
    let x = read input1 :: Bool
    
    putStrLn "Digite o segundo valor booleano (True ou False):"
    input2 <- getLine
    let y = read input2 :: Bool
    
    let result = xor x y
    putStrLn $ "O resultado do XOR é: " ++ show result
