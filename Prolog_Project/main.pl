:- use_module(geradorDeCupons).
:- use_module(menu).
:- use_module(login).
:- use_module(relatorio).  % Include the relatorio module

/* use [main]. to start the program. */
main :-
    write('Bem-vindo a caixa registradora P.L.P.'), nl,
    empty_assoc(TabelaHashCupom),
    Funcionarios = [],
    limpar_relatorio,  % Clear the report on startup
    registrar_acao('Sistema iniciado'),  % Log system startup
    menu(TabelaHashCupom, Funcionarios, none).

:- initialization(main).
