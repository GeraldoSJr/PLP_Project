:- module(geradorDeCupons, [gerar_cupom/3, verificar_cupom/2, remover_cupom/4]).
:- use_module(library(assoc)).

gerar_cupom(TabelaHashAtual, Desconto, TabelaFinal) :- 
    gerar_codigo_cupom(Cupom),
    (   get_assoc(Cupom, TabelaHashAtual, _) 
    ->  gerar_cupom(TabelaHashAtual, Desconto, TabelaFinal)
    ;   put_assoc(Cupom, TabelaHashAtual, Desconto, TabelaFinal),
        write('Novo cupom gerado: '), write(Cupom), 
        write(', Desconto: '), write(Desconto), write('%'), nl
    ).

gerar_codigo_cupom(Cupom) :- 
    random(1000, 9999, Numero), 
    atom_number(Cupom, Numero). 

verificar_cupom(Cupom, TabelaHash) :- 
    (   get_assoc(Cupom, TabelaHash, Desconto) 
    ->  format('Cupom ~w encontrado com desconto: ~w%~n', [Cupom, Desconto])
    ;   write('Cupom nao encontrado.'), nl
    ).

remover_cupom(Cupom, TabelaHashAtual, TabelaHashAtualNova, Desconto) :- 
    (   get_assoc(Cupom, TabelaHashAtual, Desconto)  % Obtém o desconto antes de remover
    ->  del_assoc(Cupom, TabelaHashAtual, TabelaHashAtualNova,_)  % Remove o cupom
    ;   TabelaHashAtualNova = TabelaHashAtual, Desconto = none  % Caso o cupom não exista
    ).
