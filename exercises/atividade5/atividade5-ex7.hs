{- Motifique o arquivo alunos.hs (disponível no Moodle) de forma a adicionar novas funções:
A: Crie uma função com a seguinte assinatura: aprovados :: [(Int, String, Float)] -> [String],
a qual recebe uma lista de alunos e retorna uma lista com o nome dos alunos aprovados. Um aluno
está aprovado se a sua média é maior ou igual a 6. Utilize map e filter para resolver esta questão.
B: Crie uma função com a seguinte assinatura: aprovados2 :: [(Int, String, Float)] -> [String],
a qual recebe uma lista de alunos e retorna uma lista com o nome dos alunos aprovados. Um aluno
está aprovado se a sua média é maior ou igual a 6. Não utilize map e filter para resolver esta
questão. Utilize o conceito de list comprehension.
C: Utilize (e modifique, se necessário) a função gerarPares vista em aula e disponível no arquivo
alunos.hs para formar duplas de alunos. Note que um aluno não pode fazer dupla consigo mesmo. -}

-- alunos.hs
alunos :: [(Int, String, Float)]
alunos = [(1, "Ana", 3.4), (2, "Bob", 6.7), (3, "Tom", 7.6)]

getNome :: (Int, String, Float) -> String
getNome (a,b,c) = b

getPrimeiroAluno :: [(Int, String, Float)] -> (Int, String, Float)
getPrimeiroAluno (a:_) = a

gerarPares :: [t] -> [u] -> [(t,u)] 
gerarPares l1 l2 = [(a,b) | a <- l1, b <- l2]

-- A:
aprovados :: [(Int, String, Float)] -> [String]
aprovados alunos = map getNome (filter (\(_,_,c) -> c >= 6) alunos)

-- B:
aprovados2 :: [(Int, String, Float)] -> [String]
aprovados2 alunos = [getNome aluno | aluno <- alunos, (\(_,_,c) -> c >= 6) aluno]

-- C:
duplas :: [(Int, String, Float)] -> [(String, String)]
duplas alunos = gerarPares (map getNome alunos) (map getNome alunos)

main = do
    print (aprovados alunos)
    print (aprovados2 alunos)
    print (duplas alunos)
