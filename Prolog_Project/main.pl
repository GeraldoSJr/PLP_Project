:- use_module(geradorDeCupons).
:- use_module(menu).
:- use_module(login).

/* use [main]. to start the program. */
main :-
    write('Bem-vindo a caixa registradora P.L.P.'), nl,
    empty_assoc(TabelaHashCupom),
    Funcionarios = [],
    menu(TabelaHashCupom, Funcionarios, none).

:- initialization(main).