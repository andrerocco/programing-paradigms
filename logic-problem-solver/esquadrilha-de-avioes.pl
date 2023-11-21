% O problema lógico a seguir pode ser acessado em: https://rachacuca.com.br/logica/problemas/esquadrilha-de-avioes-ii/


/* Definindo os fatos */

% Definindo a ordem dos aviões na formação
ordem_avioes([_, _, _, _, _]).

% Definindo o nome dos pilotos
nome(farfarelli).
nome(milton).
nome(nascimento).
nome(rui).
nome(walter).

% Definindo os estados de origem dos pilotos
gentilico(gaucho).
gentilico(baiano).
gentilico(fluminense).
gentilico(mineiro).
gentilico(paulista).

% Definindo as cores dos aviões
cor(amarelo).
cor(azul).
cor(branco).
cor(verde).
cor(vermelho).

% Definindo a anomalia de cada avião
anomalia(altimetro).
anomalia(bussola).
anomalia(hidraulico).
anomalia(radio).
anomalia(temperatura).

% Definindo as bebidas gostadas por cada piloto
bebida(agua).
bebida(cafe).
bebida(cha).
bebida(cerveja).
bebida(leite).

% Definindo os esportes praticados por cada piloto
esporte(equitacao).
esporte(futebol).
esporte(natacao).
esporte(pesca).
esporte(tenis).


/* Definindo as regras (predicados) */

% X está à ao lado de Y (em qualquer posição imediatamente à esquerda ou à direita)
aoLado(X,Y,Lista) :- nextto(X,Y,Lista);nextto(Y,X,Lista).

% X está à esquerda de Y (em qualquer posição à esquerda)
aEsquerda(X, Y, Lista) :- nth0(IndexX, Lista, X), 
                          nth0(IndexY, Lista, Y), 
                          IndexX < IndexY.

% X está à direita de Y (em qualquer posição à direita)
aDireita(X, Y, Lista) :- aEsquerda(Y, X, Lista).

% X está exatamente à esquerda de Y (uma posição à esquerda)
exatamenteAEsquerda(X, Y, Lista) :- nextto(X, Y, Lista).

% X está exatamente à direita de Y (uma posição à direita)
exatamenteADireita(X, Y, Lista) :- exatamenteAEsquerda(Y, X, Lista).

% X está entre Y e Z, nesta ordem (não necessariamente na posição imediatamente à esquerda ou à direita)
estaEntre(X, Y, Z, Lista) :- nth0(IndexY, Lista, Y),
                             nth0(IndexZ, Lista, Z),
                             nth0(IndexX, Lista, X),
                             IndexX > IndexY,
                             IndexX < IndexZ.

todosDiferentes([]).
todosDiferentes([H|T]) :- not(member(H,T)), todosDiferentes(T).

solucao(ListaSolucao) :- 
    

    ListaSolucao = [
        aviao(Nome1, Gentilico1, Cor1, Anomalia1, Bebida1, Esporte1),
        aviao(Nome2, Gentilico2, Cor2, Anomalia2, Bebida2, Esporte2),
        aviao(Nome3, Gentilico3, Cor3, Anomalia3, Bebida3, Esporte3),
        aviao(Nome4, Gentilico4, Cor4, Anomalia4, Bebida4, Esporte4),
        aviao(Nome5, Gentilico5, Cor5, Anomalia5, Bebida5, Esporte5)
    ],

    % O Cap. Farfarelli está em algum lugar à esquerda de quem gosta de praticar Futebol.
    aEsquerda(aviao(farfarelli, _, _, _, _, _), aviao(_, _, _, _, _, futebol), ListaSolucao),

    % Quem gosta de beber Água também gosta de Pescar nos finais-de-semana.
    member(aviao(_, _, _, _, agua, pesca), ListaSolucao),

    % O Cap. Walter voa em algum lugar entre os praticantes de Equitação e Tênis, nesta ordem.
    estaEntre(aviao(walter, _, _, _, _, _), aviao(_, _, _, _, _, equitacao), aviao(_, _, _, _, _, tenis), ListaSolucao),

    % O homem que joga Tênis está no meio da formação.
    % nth1(3, ListaSolucao, aviao(_, _, _, _, _, tenis)),
    Esporte3 = tenis,

    % O avião com anomalia no Rádio, o piloto que gosta de Café e o Mineiro estão nessa ordem na formação (não necessariamente um ao lado do outro).
    % Ou seja, o piloto que gosta de Café está entre o avião com anomalia no Rádio e o Mineiro.
    estaEntre(aviao(_, _, _, _, cafe, _), aviao(_, _, _, radio, _, _), aviao(_, mineiro, _, _, _, _), ListaSolucao),

    % Quem gosta de beber Chá está em algum lugar entre os pilotos que bebem Cerveja e Café, nesta ordem.
    estaEntre(aviao(_, _, _, _, cha, _), aviao(_, _, _, _, cerveja, _), aviao(_, _, _, _, cafe, _), ListaSolucao),

    % Quem bebe Cerveja está na posição mais a esquerda da formação.
    % nth1(1, ListaSolucao, aviao(_, _, _, _, cerveja, _)),
    Bebida1 = cerveja,

    % O piloto que bebe Água está ao lado do avião com anomalia na Bússola.
    aoLado(aviao(_, _, _, _, agua, _), aviao(_, _, _, bussola, _, _), ListaSolucao),

    % O avião com pane no Altímetro está em algum lugar à direita do Cel. Rui.
    aDireita(aviao(_, _, _, altimetro, _, _), aviao(rui, _, _, _, _, _), ListaSolucao),

    % Quem gosta de Futebol pilota o avião que está exatamente à direita do avião com pane no Altímetro.
    exatamenteADireita(aviao(_, _, _, _, _, futebol), aviao(_, _, _, altimetro, _, _), ListaSolucao),

    % O Cel. Rui está pilotando o avião que está em algum lugar à esquerda do avião com problemas na Bússola.
    aEsquerda(aviao(rui, _, _, _, _, _), aviao(_, _, _, bussola, _, _), ListaSolucao),

    % Os aviões com anomalias de Rádio, Hidráulico e de Temperatura se apresentam nesta ordem (não necessariamente um ao lado do outro).
    estaEntre(aviao(_, _, _, hidraulico, _, _), aviao(_, _, _, radio, _, _), aviao(_, _, _, temperatura, _, _), ListaSolucao),

    % O avião que solta fumaça Verde voa em algum lugar à direita do avião pilotado por Farfarelli.
    aDireita(aviao(_, _, verde, _, _, _), aviao(farfarelli, _, _, _, _, _), ListaSolucao),

    % O avião pilotado pelo Cel. Rui solta fumaça Branca.
    member(aviao(rui, _, branco, _, _, _), ListaSolucao),

    % O piloto que gosta de Chá pilota o avião que solta fumaça Azul.
    member(aviao(_, _, azul, _, cha, _), ListaSolucao),

    % O Paulista pilota o avião que está exatamente à esquerda do que solta fumaça Amarela.
    exatamenteAEsquerda(aviao(_, paulista, _, _, _, _), aviao(_, _, amarelo, _, _, _), ListaSolucao),

    % O Cel. Rui voa ao lado do piloto Mineiro.
    aoLado(aviao(rui, _, _, _, _, _), aviao(_, mineiro, _, _, _, _), ListaSolucao),

    % O piloto Sul-rio-grandense está exatamente à direita de quem gosta de tomar Cerveja.
    exatamenteADireita(aviao(_, gaucho, _, _, _, _), aviao(_, _, _, _, cerveja, _), ListaSolucao),

    % O piloto Fluminense pilota ao lado do avião que solta fumaça Azul.
    aoLado(aviao(_, fluminense, _, _, _, _), aviao(_, _, azul, _, _, _), ListaSolucao),

    % O Cap. Nascimento voa ao lado do piloto que gosta de tomar Chá.
    aoLado(aviao(nascimento, _, _, _, _, _), aviao(_, _, _, _, cha, _), ListaSolucao),

    /* Testando todas as possibilidades... */
    nome(Nome1), nome(Nome2), nome(Nome3), nome(Nome4), nome(Nome5),
    todosDiferentes([Nome1, Nome2, Nome3, Nome4, Nome5]),

    gentilico(Gentilico1), gentilico(Gentilico2), gentilico(Gentilico3), gentilico(Gentilico4), gentilico(Gentilico5),
    todosDiferentes([Gentilico1, Gentilico2, Gentilico3, Gentilico4, Gentilico5]),

    cor(Cor1), cor(Cor2), cor(Cor3), cor(Cor4), cor(Cor5),
    todosDiferentes([Cor1, Cor2, Cor3, Cor4, Cor5]),

    anomalia(Anomalia1), anomalia(Anomalia2), anomalia(Anomalia3), anomalia(Anomalia4), anomalia(Anomalia5),
    todosDiferentes([Anomalia1, Anomalia2, Anomalia3, Anomalia4, Anomalia5]),

    bebida(Bebida1), bebida(Bebida2), bebida(Bebida3), bebida(Bebida4), bebida(Bebida5),
    todosDiferentes([Bebida1, Bebida2, Bebida3, Bebida4, Bebida5]),

    esporte(Esporte1), esporte(Esporte2), esporte(Esporte3), esporte(Esporte4), esporte(Esporte5),
    todosDiferentes([Esporte1, Esporte2, Esporte3, Esporte4, Esporte5]).
