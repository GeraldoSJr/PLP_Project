:- module(login, [criar_login/5, efetuar_login/4]).


funcao(gerente).
funcao(caixa).


criar_login(NomeUsuario, Senha, Funcao, Funcionarios, [funcionario(NomeUsuario, Senha, Funcao) | Funcionarios]) :-
    \+ member(funcionario(NomeUsuario, _, _), Funcionarios),
    funcao(Funcao).


efetuar_login(NomeUsuario, Senha, [funcionario(NomeUsuario, Senha, Funcao) | _], Funcao) :- !.
efetuar_login(NomeUsuario, Senha, [_ | RestoFuncionarios], Funcao) :-
    efetuar_login(NomeUsuario, Senha, RestoFuncionarios, Funcao).
efetuar_login(_, _, [], none).


efetuar_login(NomeUsuario, Senha, Funcionarios, Resultado) :-
    ( efetuar_login(NomeUsuario, Senha, Funcionarios, Funcao) ->
        Resultado = Funcao
    ; Resultado = none
    ).
