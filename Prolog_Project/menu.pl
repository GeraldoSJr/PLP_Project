:- module(menu, [menu/1]).

menu(TabelaHashCupom) :-
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
    write('10. Aplicar desconto no produto'), nl,
    write('Escolha uma opcao: '),
    read(Opcao),
    executar(Opcao, TabelaHashCupom).

executar(1, TabelaHashCupom) :- 
    menu(TabelaHashCupom).

executar(2, TabelaHashCupom) :- 
    menu(TabelaHashCupom).

executar(3, TabelaHashCupom) :-     
    menu(TabelaHashCupom).

executar(4, TabelaHashCupom) :- 
    menu(TabelaHashCupom).

executar(5, TabelaHashCupom) :- 
    menu(TabelaHashCupom).

executar(6, TabelaHashCupom) :- 
    menu(TabelaHashCupom).

executar(7, TabelaHashCupom) :- 
    menu(TabelaHashCupom).

executar(8, TabelaHashCupom) :-
    write('Digite a porcentagem de desconto: '),
    read(Desconto),
    gerar_cupom(TabelaHashCupom, Desconto, NovaTabelaHash),  
    menu(NovaTabelaHash).

executar(9, TabelaHashCupom) :-
    write('Digite o codigo do cupom: '),
    read(CupomInput),
    atom_number(CupomAtom, CupomInput),
    verificar_cupom(CupomAtom, TabelaHashCupom),
    menu(TabelaHashCupom).

executar(10, TabelaHashCupom) :-
    write('Digite o codigo do cupom: '),
    read(CupomInput),
    atom_number(CupomAtom, CupomInput),
    pegar_desconto_cupom(CupomAtom, TabelaHashCupom, Desconto),
    remover_cupom(CupomAtom, TabelaHashCupom, NovaTabelaHash),
    menu(TabelaHashCupom).


executar(_, TabelaHashCupom) :-
    write('Opcao invalida! Tente novamente.'), nl,
    menu(TabelaHashCupom).