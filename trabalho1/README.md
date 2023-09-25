# Kojun Solver

## Regras do jogo

O jogo Kojun é um jogo de lógica, onde, dado um tabuleiro cujas célular são divididas em regiões, deve-se:

1. Inserir um número em cada célula da grade para que cada região de tamanho N contenha cada número de 1 a N exatamente uma vez.
2. Os números em células ortogonalmente adjacentes devem ser diferentes.
3. Se duas células são adjacentes verticalmente na mesma região, o número na célula superior deve ser maior que o número na célula inferior.

## Passos de solução

Primeiramente, deve-se realizar a varredura do tabuleiro para verificar se existem posições cuja solução é obvia. Então, para cada célula na matriz, deve-se:

1.  Verificar se aquela célula pertence a uma região unitária (com apenas uma célula). Se sim, inserir o número 1 naquela célula.
