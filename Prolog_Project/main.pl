:- use_module(geradorDeCupons).

:- use_module(menu).

/* use [start]. para iniciar o programa.*/
main :-
    write('Bem-vindo a caixa registradora P.L.P.'), nl,
    empty_assoc(TabelaHashCupom),
    menu(TabelaHashCupom).

:- main.