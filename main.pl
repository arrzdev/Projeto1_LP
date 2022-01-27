/*
Aluno: Andre Filipe Silva Santos
Numero de Aluno: 103597
E-mail: andrefssantos@tecnico.ulisboa.pt
*/

:- [codigo_comum].
%:-[puzzles_publicos].

%extrai_ilhas/3
/*
N_L eh um inteiro positivo, correspondente ao numero de uma linha e Linha eh
uma lista, correspondente a uma linha de um puzzle, significa que Ilhas eh a
lista ordenada (ilhas da esquerda para a direita) cujos elementos sao as 
ilhas da linha Linha.
*/
extrai_ilhas_linha(N_L, Linha, Ilhas) :-
    extrai_ilhas_linha(N_L, 1, Linha, Ilhas).

extrai_ilhas_linha(_, _, [], []) :- !.

extrai_ilhas_linha(N_L, N_C, [P | R], Ilhas) :-
    (
        P \== 0 ->
        Ilhas = [ilha(P, (N_L, N_C)) | AuxIlhas];
        Ilhas = AuxIlhas
    ),
    New_N_C is N_C + 1,
    extrai_ilhas_linha(N_L, New_N_C, R, AuxIlhas).

%ilhas/2
/*
Puz e um puzzle, significa que Ilhas eh a lista ordenada (ilhas da esquerda 
para a direita e de cima para baixo) cujos elementos sao as ilhas de Puz.
*/
ilhas(Puz, Ilhas) :-
    ilhas(1, [], Puz, Ilhas).

ilhas(_, AuxLista, [], AuxLista) :- !.

ilhas(N_L, AuxLista, [P | R], Ilhas) :-
    extrai_ilhas_linha(N_L, P, L_Ilhas),
    append(AuxLista, L_Ilhas, F_Ilhas),
    New_N_L is N_L + 1,
    ilhas(New_N_L, F_Ilhas, R, Ilhas).

%vizinhas/3
/*
Ilhas eh a lista de ilhas de um puzzle e Ilha eh uma dessas ilhas, significa
que Vizinhas eh a lista ordenada (ilhas de cima para baixo e da esquerda 
para a direita) cujos elementos sao as ilhas vizinhas de Ilha.
*/
vizinhas(Ilhas, Ilha, Vizinhas) :-
    Ilha = ilha(_, (CLinha, CColuna)),

    %Encontrar as ilhas da mesma Ilha
    findall(LIlha, (member(LIlha, Ilhas), ilha(_, (Linha, _)) = LIlha, 
    Linha =:= CLinha), IlhasLinha),
    
    %Encontrar as ilhas da mesma Coluna
    findall(LIlha, (member(LIlha, Ilhas), ilha(_, (_, Coluna)) = LIlha, 
    Coluna =:= CColuna), IlhasColuna),

    %Encontrar as vizinhas de forma ordenada (C,E,D,B)
    findall(X, nextto(X, Ilha, IlhasColuna), VizinhaCima),
    findall(X, nextto(X, Ilha, IlhasLinha), VizinhaEsquerda),
    findall(X, nextto(Ilha, X, IlhasLinha), VizinhaDireita),
    findall(X, nextto(Ilha, X, IlhasColuna), VizinhaBaixo),

    %Alisar
    append([VizinhaCima, VizinhaEsquerda, VizinhaDireita, VizinhaBaixo], Vizinhas).

%estado/2
/*
Ilhas eh a lista de ilhas de um puzzle, significa que Estado eh a lista 
ordenada cujos elementos sao as entradas referentes a cada uma das ilhas 
de Ilhas.
*/
estado(Ilhas, Estado) :-
    findall([Ilha, Vizinhas, []], (member(Ilha, Ilhas), 
    vizinhas(Ilhas, Ilha, Vizinhas)), Estado).

%posicoes_entre/3
/*
Pos1 e Pos2 sao posicoes, significa que Posicoes eh a lista ordenada de 
posicoes entre Pos1 e Pos2 (excluindo Pos1 e Pos2). Se Pos1 e Pos2 nao 
pertencerem a mesma linha ou a mesma coluna, o resultado eh false.
*/
posicoes_entre(Pos1, Pos2, Posicoes) :-    
    %Retirar as cordenadas das Pos1 e Pos2 de forma ordenada
    sort([Pos1, Pos2], [(L1, C1), (L2, C2)]),

    (
        L1 =:= L2 ->
        findall((L1,X), between(C1, C2, X), AuxPosicoes);
        C1 =:= C2,
        findall((X, C1), between(L1, L2, X), AuxPosicoes)
    ),

    %Retirar as posicoes
    subtract(AuxPosicoes, [Pos1, Pos2], Posicoes).
    
%cria_ponte/3
/*
Pos1 e Pos2 sao 2 posicoes, significa que Ponte eh uma ponte entre essas 2 
posicoes. 
*/
cria_ponte(Pos1, Pos2, ponte(SortedPos1, SortedPos2)) :-
    sort([Pos1, Pos2], [SortedPos1, SortedPos2]).

%caminho_livre/5
/*
Pos1 e Pos2 sao posicoes, Posicoes eh a lista ordenada de posicoes entre 
Pos1 e Pos2, I eh uma ilha, e Vz eh uma das suas vizinhas, significa que a 
adicao da ponte ponte(Pos1, Pos2) nao faz com que I e Vz deixem de ser 
vizinhas.
*/
caminho_livre(Pos1, _, Posicoes, ilha(_, PosIlha), ilha(_, PosVizinha)) :-
    posicoes_entre(PosIlha, PosVizinha, Posicoes_entre_Vizinha),
    intersection(Posicoes_entre_Vizinha, Posicoes, Intersection),

    (
    Intersection == [];
    Intersection == Posicoes,
    member(Pos1, [PosIlha, PosVizinha])
    ).   
    
%actualiza_vizinhas_entrada/5
/*
Pos1 e Pos2 sao as posicoes entre as quais ira ser adicionada uma ponte, 
Posicoes eh a lista ordenada de posicoes entre Pos1 e Pos2, e Entrada eh uma
entrada, significa que Nova_Entrada eh igual a Entrada, excepto no que diz 
respeito a lista de ilhas vizinhas; Esta eh atualizada, removendo as ilhas 
que deixaram de ser vizinhas, apos a adicao da ponte.
*/
actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, [Ilha, Vizinhas, Pontes], [Ilha, NovasVizinhas, Pontes]) :-
    include(caminho_livre(Pos1, Pos2, Posicoes, Ilha), Vizinhas, NovasVizinhas).

%actualiza_vizinhas_apos_pontes/4
/*
Estado eh um estado, Pos1 e Pos2 sao as posicoes entre as quais foi 
adicionada uma ponte, significa que Novo_estado eh o estado que se obtem de
Estado apos a actualizacao das ilhas vizinhas de cada uma das suas entradas.
*/
actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Novo_Estado) :-
    posicoes_entre(Pos1, Pos2, Posicoes),
    maplist(actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes), Estado, Novo_Estado).
    
%ilhas_terminadas/2
/*
Estado eh um estado, significa que Ilhas_term eh a lista de ilhas que ja tem
todas as pontes associadas, designadas por ilhas terminadas. Se a entrada 
referente a uma ilha for [ilha(N_pontes, Pos), Vizinhas, Pontes], esta ilha 
esta terminada se N_pontes for diferente de 'X' (a razao para esta condicao 
ficara aparente mais a frente) e o comprimento da lista Pontes for N_pontes.
*/
ilhas_terminadas(Estado, Ilhas_term) :-
    findall(ilha(N_Pontes, Pos),
    (member([ilha(N_Pontes, Pos), _, Pontes], Estado),
    N_Pontes \== 'X',
    length(Pontes, N_Pontes)), Ilhas_term).

%tira_ilhas_terminadas_entrada/3
/*
Ilhas_term eh uma lista de ilhas terminadas e Entrada eh uma entrada, 
significa que Nova_entrada eh a entrada resultante de remover as ilhas de 
Ilhas_term, da lista de ilhas vizinhas de entrada
*/
tira_ilhas_terminadas_entrada(Ilhas_term, [Ilha, Vizinhas, Pontes], [Ilha, AuxVizinhas, Pontes]) :-
    findall(Vizinha, (member(Vizinha, Vizinhas), \+(member(Vizinha, Ilhas_term))), AuxVizinhas).

%tira_ilhas_terminadas/3
/*
Estado eh um estado e Ilhas_term eh uma lista de ilhas terminadas, significa
que Novo_estado eh o estado resultante de aplicar o predicado 
tira_ilhas_terminadas_entrada a cada uma das entradas de Estado.
*/
tira_ilhas_terminadas(Estado, Ilhas_term, Novo_Estado) :-
    findall(Nova_Entrada, (member(Entrada, Estado), 
    tira_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_Entrada)), Novo_Estado).

%marca_ilhas_terminadas_entrada/3
/*
Ilhas_term eh uma lista de ilhas terminadas e Entrada eh uma entrada, 
significa que Nova_entrada eh a entrada obtida de Entrada da seguinte forma:
se a ilha de Entrada pertencer a Ilhas_term, o numero de pontes desta eh 
substituido por 'X'; em caso contrario Nova_entrada eh igual a Entrada.
*/
marca_ilhas_terminadas_entrada(Ilhas_term, [ilha(N_Pontes, Posicao), Vizinhas, Pontes], Nova_Entrada) :-
    (
        member(ilha(N_Pontes, Posicao), Ilhas_term) ->
        Nova_Entrada = [ilha('X', Posicao), Vizinhas, Pontes];
        Nova_Entrada = [ilha(N_Pontes, Posicao), Vizinhas, Pontes]
    ).
    
%marca_ilhas_terminadas/3
/*
Estado eh um estado e Ilhas_term eh uma lista de ilhas terminadas, significa
que Novo_estado eh o estado resultante de aplicar o predicado 
marca_ilhas_terminadas_entrada a cada uma das entradas de Estado.
*/
marca_ilhas_terminadas(Estado, Ilhas_term, Novo_Estado) :-
    findall(Nova_Entrada, (member(Entrada, Estado), 
    marca_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_Entrada)), Novo_Estado).

%trata_ilhas_terminadas/2
/*
Estado eh um estado, significa que Novo_estado eh o estado resultante de 
aplicar os predicados tira_ilhas_terminadas e marca_ilhas_terminadas a Estado.
*/
trata_ilhas_terminadas(Estado, Novo_Estado) :-

    ilhas_terminadas(Estado, Ilhas_term),
    marca_ilhas_terminadas(Estado, Ilhas_term, AuxNovoEstado),
    tira_ilhas_terminadas(AuxNovoEstado, Ilhas_term, Novo_Estado).

%aux_junta_pontes
/*
Pontes_Add sao as pontes a ser adicionadas, Ilhas sao as ilhas as quais queremos 
adicionar, Entrada e a entrada com as nossas ilhas e Nova_Entrada e a Entrada.
*/
aux_junta_pontes(Pontes_Add, Ilhas, [Ilha, Vizinhas, Pontes], Nova_Entrada) :-    
    (
    member(Ilha, Ilhas),
    append(Pontes_Add, Pontes, Novas_Pontes);
    Novas_Pontes = Pontes
    ),

    Nova_Entrada = [Ilha, Vizinhas, Novas_Pontes].    

%junta_pontes/5
/*
Estado eh um estado e Ilha1 e Ilha2 sao 2 ilhas, significa que Novo_estado 
eh o estado que se obtem de Estado por adicao de Num_pontes pontes entre 
Ilha1 e Ilha2.
*/
junta_pontes(Estado, Num_pontes, Ilha1, Ilha2, Novo_Estado) :-
    Ilha1 = ilha(_, Posicao1),
    Ilha2 = ilha(_, Posicao2),

    cria_ponte(Posicao1, Posicao2, Ponte),

    %Criar uma lista com o numero de pontes necessario
    length(Pontes, Num_pontes), 
    maplist(=(Ponte), Pontes),
    
    %Adicionar as pontes as ilhas
    maplist(aux_junta_pontes(Pontes, [Ilha1, Ilha2]), Estado, Aux1_Novo_Estado),
    
    actualiza_vizinhas_apos_pontes(Aux1_Novo_Estado, Posicao1, Posicao2, 
    Aux2_Novo_Estado),
    trata_ilhas_terminadas(Aux2_Novo_Estado, Novo_Estado).