# paradigmas

## CONWAY'S GAME OF LIFE

Consiste em fazer um Autômato Celular do Jogo da Vida criado por John Conway em 1970.
O Jogo foi criado como uma forma de reproduzir as alterações e mudanças em grupos de seres vivos, a partir de um conjunto de regras simples. A partir de uma configuração inicial de células, pode se observar a evolução dos estados das células através da passagem do tempo, de acordo com as regras definidas.

Regras:
1) Qualquer célula viva com menos de dois vizinhos vivos morre de solidão.
2) Qualquer célula viva com mais de três vizinhos vivos morre de superpopulação.
3) Qualquer célula morta com exatamente três vizinhos vivos se torna uma célula viva.
4) Qualquer célula viva com dois ou três vizinhos vivos continua no mesmo estado para a próxima geração.

foi utilizada a versão 7.10.3 do ghci.

para rodar o programa entre na pasta do projeto e digite:

    ghci game-of-life.hs
    main

ou caso já esteja com o ghci executando digite:

    :l game-of-life.hs
    main


### Integrantes

Matricula - Nome

160010331 - José Aquiles

160154197 - Rossicler Junior
