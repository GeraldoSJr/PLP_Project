:- use_module(geradorDeCupons).

main :-
    write('Bem-vindo a caixa registradora P.L.P.'), nl,
    empty_assoc(TabelaHashCupom),
    menu(TabelaHashCupom).

:- main.