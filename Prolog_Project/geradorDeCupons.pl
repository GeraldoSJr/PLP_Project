:- module(geradorDeCupons, [gerar_cupom/3, verificar_cupom/2]).

:- use_module(library(assoc)).

gerar_cupom(TabelaHashAtual, Desconto, TabelaFinal) :-
    gerar_codigo_cupom(Cupom),
    (   get_assoc(Cupom, TabelaHashAtual, _)
    ->  gerar_cupom(TabelaHashAtual, Desconto, TabelaFinal)
    ;   put_assoc(Cupom, TabelaHashAtual, Desconto, TabelaFinal),
        write('Novo cupom gerado: '), write(Cupom), 
        write(', Desconto: '), write(Desconto),write('%'), nl
    ).

gerar_codigo_cupom(Cupom) :-
    random(1000, 9999, Numero),
    atom_number(Cupom, Numero). 

verificar_cupom(Cupom, TabelaHash) :-
    (   get_assoc(Cupom, TabelaHash, Desconto)
    ->  format('Cupom ~w encontrado com desconto: ~w%~n', [Cupom, Desconto])
    ;   write('Cupom nao encontrado.'), nl
    ).

remover_cupom(Cupom, TabelaHashAtual, TabelaHashAtualNova) :-
    del_assoc(Cupom, TabelaHashAtual, TabelaHashAtualNova).

pegar_desconto_cupom(Cupom, TabelaHash, Desconto) :-
    get_assoc(Cupom, TabelaHash, Desconto).