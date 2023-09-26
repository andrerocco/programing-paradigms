# Kojun Solver

O jogo Kojun é um jogo de lógica, onde, dado um tabuleiro cujas célular são divididas em regiões, deve-se:

1. Inserir um número em cada célula da grade para que cada região de tamanho N contenha cada número de 1 a N exatamente uma vez.
2. Os números em células ortogonalmente adjacentes devem ser diferentes.
3. Se duas células são adjacentes verticalmente na mesma região, o número na célula superior deve ser maior que o número na célula inferior.

O problema proposto é a criação de um programa que, dado um tabuleiro de Kojun, encontre uma solução para o mesmo. Existem diferentes abordagens para a resolução desse problema, algumas mais performáticas e outras mais entendíveis. Neste desenvolvimento, foi escolhida uma abordagem que, apesar de não ser a mais performática, é relativamente simples de ser entendida: a técnica de backtracking.

## Solução proposta

### Modelagem do tabuleiro

Um tabuleiro Kojun consiste em uma grade (onde podem ser colocados valores) dividida em diferentes regiões. Para representar computacionalmente um tabuleiro, foram criadas duas matrizes:

1. Uma matriz de valores inteiros, onde cada célula representa uma posição do tabuleiro e o valor da célula representa o valor que está naquela posição. Em seu estado inicial, o tabuleiro pode conter valores ou não. Caso não contenha valores (células que devem ser preenchidas pelo jogador para completar o jogo), o valor da célula é 0.

```
2 0 0 0 1 0
0 0 0 3 0 0
0 3 0 0 5 3
0 0 0 0 0 0
0 0 3 0 4 2
0 0 0 0 0 0
```

2. Uma matriz de caracteres, onde cada célula representa uma posição do tabuleiro e o valor da célula representa a região daquela posição. Note que células da mesma região precisam sempre estar conectadas, ou seja, não podem existir células da mesma região que não sejam ortogonalmente adjacentes.

```
a a b b b c
d d d d d c
e f f f d g
e e e f g g
h h i j j j
k k i i j j
```

Para serem utilizados pelo programa, os tabuleiros foram escritos em arquivos .txt onde a primeira linha do arquivo indica o tamanho N do tabuleiro (N linhas por N colunas). As próximas N linhas se referem à matriz de valores e as N linhas após a matriz de valores se referem à matriz de regiões. Não há restrições quanto ao tamanho do tabuleiro: o algoritmo é capaz de resolver tabuleiros Kujon de qualquer tamanho. Entretanto, na implementação realizada cada região deve ser representada com apenas um caractere gerando então uma limitação quanto à quantidade de regiões.
Para o suporte à estrutura de matriz no Haskell, foi optado pela criação de um módulo Matrix que inicializa a estrutura e implementa algumas das operações básicas que seriam necessárias para a resolução do problema.

### Inicialização do programa

O programa principal inicia lendo e extraindo as matrizes do arquivos .txt especificado em contents. A partir das linhas do arquivo de texto, inicializa as estruturas do tipo Matrix que correspondem à matriz de valores e à matriz de regiões. Em seguida, é chamada a função solveKojun que aplicará o algoritmo de backtracking desenvolvido sobre o tabuleiros (representado pelas duas matrizes).

### Algoritmo de backtracking

O algoritmo de backtracking é uma técnica de busca recursiva que tenta encontrar uma solução para um problema através de tentativa e erro, retrocedendo (backtrack) quando uma tentativa falha. Começando de uma configuração inicial, o algoritmo faz uma escolha dentre várias possíveis e avança na solução. Se chegar a um ponto onde não é possível continuar sem violar uma condição ou restrição, ele retrocede para a escolha anterior e tenta outra opção. Esse processo recursivo continua até que uma solução seja encontrada ou todas as possibilidades tenham sido exploradas.
O módulo KojunSolver contém as funções responsáveis por aplicar o algoritmo de backtracking. Seu ponto de entrada é a função recursiva solveKojun. Ela testa as condições de parada ou continuidade do algoritmo de backtracking e, no caso de continuidade, faz uma chamada recursiva para si mesma com a próxima posição do tabuleiro a ser resolvida (possivelmente).
A função tryValues faz a tentativa de cada um dos valores de 1 a maxRegionSize para uma célula do tabuleiro. A validação se um valor é válido ou não em uma célula do tabuleiro é feita por isValidPlacement. Essa função, em sua essência, consiste implementação a validação de jogadas baseada nas [regras do jogo](#kojun-solver).
Note que para validar corretamente se uma jogada é correta ou não, ter como informação os valores das posições adjacentes (os quais podem ser facilmente obtidos utilizando a função auxiliar getMatrixValue implementada pelo módulo Matrix) e precisamos também das outras posições do tabuleiro que estão na mesma região. Para obtermos a segunda, foi desenvolvida a função getRegionFromPosition.

### Otimizações realizadas

Para cada célula vazia do tabuleiro, é chamada a função isValidPlacement é chamada para cada um dos valores possíveis (em ordem crescente). Dessa forma, a função isValidPlacement mostra-se de grande impacto no desempenho do algoritmo de backtracking.
A função isValidPlacement chama getRegionFromPosition toda vez que é executada. Na primeira implementação realizada, a função getRegionFromPosition position varria todas as células da matriz de regiões para gerar a lista de posições (linha, coluna) de uma região. A partir disso, a função foi otimizada para realizar uma busca recursiva de posições da região a partir de um ponto, visitando assim apenas K células (onde K é o número de elementos daquela região).
Seria possível otimizar mais ainda a execução de isValidPlacement gerando um hashmap com todas as posições de cada uma regiões apenas uma vez antes de sua execução e passando essa informação como argumento isValidPlacement. Entretanto, como essa otimização envolveria um aumento da complexidade da implementação e como o desempenho não era uma preocupação para essa proposta, essa otimização não foi realizada.

## Execução do programa

Para executar o programa, é necessário ter o [Glasgow Haskell Compiler](https://www.haskell.org/ghc/) instalado. Em seguida, basta executar o comando `make run` para compilar e executar o programa. Após a compilação, é possível apenas executar o programa com o comando `./main`.
