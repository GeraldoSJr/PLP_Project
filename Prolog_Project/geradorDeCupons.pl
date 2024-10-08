% arquivo: geradorDeCupons.pl
:- module(geradorDeCupons, [
    gerar_cupom/2, 
    verificar_cupom/1, 
    remover_cupom/2, 
    salvar_cupons/0, 
    iniciar_cupons/0
]).
:- use_module(library(assoc)).
:- use_module(item).
:- use_module(library(random)).

:- dynamic tabela_cupons/1.

% Inicializa a tabela de cupons
iniciar_cupons :-
    ( tabela_cupons(_) ->
        true
    ;   
        ( load_cupons ->
            true
        ;
            empty_assoc(Assoc),
            assertz(tabela_cupons(Assoc)),
            salvar_cupons
        )
    ).

% Carrega os cupons do arquivo se existir
load_cupons :-
    exists_file('cupons.pl'),
    consult('cupons.pl').

% Salva a tabela de cupons no arquivo.
salvar_cupons :-
    tabela_cupons(Assoc),
    open('cupons.pl', write, Stream),
    portray_clause(Stream, tabela_cupons(Assoc)),
    close(Stream).

% Gera um novo cupom com um desconto especificado.
gerar_cupom(Desconto, Cupom) :- 
    tabela_cupons(TabelaAtual),
    gerar_cupom_recursivo(TabelaAtual, Desconto, NovaTabela, Cupom),
    retract(tabela_cupons(TabelaAtual)),
    assertz(tabela_cupons(NovaTabela)),
    salvar_cupons.

gerar_cupom_recursivo(TabelaAtual, Desconto, TabelaFinal, Cupom) :- 
    gerar_codigo_cupom(Cupom),
    (   get_assoc(Cupom, TabelaAtual, _) 
    ->  gerar_cupom_recursivo(TabelaAtual, Desconto, TabelaFinal, Cupom)  % Recursão se o cupom já existir
    ;   put_assoc(Cupom, TabelaAtual, Desconto, TabelaFinal),
        write('Novo cupom gerado: '), write(Cupom), 
        write(', Desconto: '), write(Desconto), write('%'), nl
    ).

% Gera um código de cupom aleatório entre 1000 e 9999.
gerar_codigo_cupom(Cupom) :- 
    random(1000, 9999, Numero), 
    atom_number(Cupom, Numero). 

% Verifica se um cupom existe e retorna seu desconto.
verificar_cupom(Cupom, Desconto) :- 
    tabela_cupons(Tabela),
    (   get_assoc(Cupom, Tabela, Desconto) 
    ->  format('Cupom ~w encontrado com desconto: ~w%~n', [Cupom, Desconto])
    ;   write('Cupom não encontrado.'), nl,
        fail
    ).

% Remove um cupom da tabela e retorna o desconto associado.
remover_cupom(Cupom, Desconto) :- 
    tabela_cupons(TabelaAtual),
    (   get_assoc(Cupom, TabelaAtual, Desconto) 
    ->  del_assoc(Cupom, TabelaAtual, NovaTabela, _),
        retract(tabela_cupons(TabelaAtual)),
        assertz(tabela_cupons(NovaTabela)),
        salvar_cupons,
        write('Cupom ~w removido com desconto: ~w%~n', [Cupom, Desconto])
    ;   write('Cupom não encontrado.'), nl,
        fail
    ).


