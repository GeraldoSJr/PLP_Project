/* rodar no swipl, com o comando [menu].*/
menu :-
    write('--- Menu Principal ---'), nl,
    write('1. '), nl,
    write('2. '), nl,
    write('3. '), nl,
    write('4. '), nl,
    write('5. '), nl,
    write('6. '), nl,
    write('7. '), nl,
    write('8. Criar cupom desconto '), nl,
    write('9. Verificar cupom de desconto '), nl,
    write('Escolha uma opcao: '),
    read(Opcao),
    executar(Opcao).

executar(1) :-
    menu.  
executar(2) :-
    menu. 
executar(3) :-
    write('Saindo do programa...'), nl.
executar(4) :-
    menu.
executar(5) :-
    menu.
executar(6) :-
    menu.
executar(7) :-
    menu.
executar(8) :-
    menu.
executar(9) :-
    menu.
executar(_) :-
    write('Opcao invalida! Tente novamente.'), nl,
    menu.

:- menu.
