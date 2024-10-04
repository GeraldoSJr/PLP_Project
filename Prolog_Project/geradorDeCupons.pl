:- module(geradorDeCupons, [gerar_cupom/3, armazenar_tabela/1]).

:- use_module(library(assoc)).

gerar_cupom(TabelaHashAtual, Desconto, TabelaFinal) :-
    gerar_codigo_cupom(Cupom),
    (   get_assoc(Cupom, TabelaHashAtual, _)
    ->  gerar_cupom(TabelaHashAtual, Desconto, TabelaFinal)
    ;   put_assoc(Cupom, TabelaHashAtual, desconto(Desconto), TabelaFinal),
        write('Novo cupom gerado: '), write(Cupom), nl
    ).

gerar_codigo_cupom(Cupom) :-
    random(1000, 9999, Numero),
    atom_concat('CUP-', Numero, Cupom).

armazenar_tabela(TabelaHash) :-
    write('Tabela final de cupons atualizada:'), nl,
    forall(
        (   assoc_to_list(TabelaHash, List),
            member(Key-Value, List)),
        (   write(Key), write(': '), write(Value), nl)
    ).