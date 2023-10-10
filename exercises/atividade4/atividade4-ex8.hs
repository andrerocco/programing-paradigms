{- Crie uma função que resolva uma equação de segundo grau da forma ax2 + bx + c utilizando a fórmula
de Bhaskara. Leia os coeficientes a, b e c do teclado. -}

bhaskara :: Double -> Double -> Double -> (Double, Double)
bhaskara a b c
    | delta < 0 = error "Não existem raízes reais"
    | otherwise = ((-b + sqrt delta) / (2 * a), (-b - sqrt delta) / (2 * a))
    where
        delta = b * b - 4 * a * c

main :: IO ()
main = do
    putStrLn "Digite o coeficiente a:"
    inputA <- getLine
    let a = read inputA :: Double
    
    putStrLn "Digite o coeficiente b:"
    inputB <- getLine
    let b = read inputB :: Double
    
    putStrLn "Digite o coeficiente c:"
    inputC <- getLine
    let c = read inputC :: Double
    
    let (raiz1, raiz2) = bhaskara a b c
    putStrLn $ "As raízes da equação são: " ++ show raiz1 ++ " e " ++ show raiz2
