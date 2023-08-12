# Solicitaram para que você construisse um programa simples de criptografia. Este programa deve
# possibilitar enviar mensagens codificadas sem que alguém consiga lê-las. O processo é muito simples.
# São feitas três passadas em todo o texto.

# Na primeira passada, somente caracteres que sejam letras minúsculas e maiúsculas devem ser deslocadas
# 3 posições para a direita, segundo a tabela ASCII: letra 'a' deve virar letra 'd', letra 'y' deve
# virar caractere '|' e assim sucessivamente. Na segunda passada, a linha deverá ser invertida. Na
# terceira e última passada, todo e qualquer caractere a partir da metade em diante (truncada) devem
# ser deslocados uma posição para a esquerda na tabela ASCII. Neste caso, 'b' vira 'a' e 'a' vira '`'.

# Por exemplo, se a entrada for “Texto #3”, o primeiro processamento sobre esta entrada deverá produzir
# “Wh{wr #3”. O resultado do segundo processamento inverte os caracteres e produz “3# rw{hW”. Por último, 
# com o deslocamento dos caracteres da metade em diante, o resultado final deve ser “3# rvzgV”.

# Entrada
# A entrada contém vários casos de teste. A primeira linha de cada caso de teste contém um inteiro N
# (1 ≤ N ≤ 1*104), indicando a quantidade de linhas que o problema deve tratar. As N linhas contém cada
# uma delas M (1 ≤ M ≤ 1*103) caracteres.

# Saída
# Para cada entrada, deve-se apresentar a mensagem criptografada.

def criptografar(texto):
    texto_criptografado = ""
    
    for char in texto:
        if char.isalpha():
            if char.islower():
                novo_char = chr((ord(char) - ord('a') + 3) % 26 + ord('a'))
            else:
                novo_char = chr((ord(char) - ord('A') + 3) % 26 + ord('A'))
            texto_criptografado += novo_char
        else:
            texto_criptografado += char
    
    texto_criptografado = texto_criptografado[::-1]
    
    meio = len(texto_criptografado) // 2
    texto_criptografado = texto_criptografado[:meio] + ''.join(chr(ord(char) - 1) for char in texto_criptografado[meio:])
    
    return texto_criptografado

num_casos = int(input())

for _ in range(num_casos):
    linha = input()
    mensagem_criptografada = criptografar(linha)
    print(mensagem_criptografada)