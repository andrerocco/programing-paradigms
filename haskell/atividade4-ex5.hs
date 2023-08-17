{- Crie uma função que receba três notas de um aluno (a, b, c), calcule a média e retorne se o aluno foi
aprovado ou reprovado. Para um aluno ser aprovado, ele deve possuir nota igual ou superior a 6. Leia as
notas dos alunos do teclado. -}

aprovadoOuReprovado :: Double -> Double -> Double -> String
aprovadoOuReprovado a b c
    | media >= 6.0 = "Aprovado"
    | otherwise    = "Reprovado"
  where
    media = (a + b + c) / 3.0

main :: IO ()
main = do
    putStrLn "Digite a primeira nota do aluno:"
    input1 <- getLine
    let nota1 = read input1 :: Double
    
    putStrLn "Digite a segunda nota do aluno:"
    input2 <- getLine
    let nota2 = read input2 :: Double
    
    putStrLn "Digite a terceira nota do aluno:"
    input3 <- getLine
    let nota3 = read input3 :: Double
    
    let resultado = aprovadoOuReprovado nota1 nota2 nota3
    putStrLn $ "O aluno está " ++ resultado