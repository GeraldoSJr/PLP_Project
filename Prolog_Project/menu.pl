:- module(menu, [menu/3]).
:- use_module(geradorDeCupons).
:- use_module(login).

menu(TabelaHashCupom, Funcionarios, FuncaoAtual) :- 
    write('--- Menu Principal ---'), nl,
    write('1. Criar login de acesso'), nl,
    write('2. Efetuar login'), nl,
    write('3. '), nl,
    write('4. '), nl,
    write('5. '), nl,
    write('6. '), nl,
    write('7. '), nl,
    write('8. Criar cupom desconto '), nl,
    write('9. Verificar cupom de desconto '), nl,
    write('10. Aplicar desconto no produto'), nl,
    write('11. Sair'), nl,
    write('Escolha uma opcao: '),
    read(Opcao),
    executar(Opcao, TabelaHashCupom, Funcionarios, FuncaoAtual).

executar(1, TabelaHashCupom, Funcionarios, FuncaoAtual) :- 
    write('Digite o nome de usuário: '), read(NomeUsuario),
    write('Digite a senha: '), read(Senha),
    write('Digite a função (gerente/caixa): '), read(Funcao),
    (criar_login(NomeUsuario, Senha, Funcao, Funcionarios, NovosFuncionarios) ->
        write('Login criado com sucesso!'), nl,
        menu(TabelaHashCupom, NovosFuncionarios, FuncaoAtual)
    ;
        write('Falha ao criar login. Verifique os dados.'), nl,
        menu(TabelaHashCupom, Funcionarios, FuncaoAtual)
    ).

executar(2, TabelaHashCupom, Funcionarios, _) :- 
    write('Digite o nome de usuário: '), read(NomeUsuario),
    write('Digite a senha: '), read(Senha),
    efetuar_login(NomeUsuario, Senha, Funcionarios, NovaFuncao),
    (NovaFuncao \= none ->
        write('Login efetuado com sucesso como: '), write(NovaFuncao), nl,
        menu(TabelaHashCupom, Funcionarios, NovaFuncao)
    ; 
        write('Falha no login. Verifique suas credenciais.'), nl,
        menu(TabelaHashCupom, Funcionarios, none)
    ).

executar(3, TabelaHashCupom, Funcionarios, FuncaoAtual) :-     
    menu(TabelaHashCupom, Funcionarios, FuncaoAtual).

executar(4, TabelaHashCupom, Funcionarios, FuncaoAtual) :- 
    menu(TabelaHashCupom, Funcionarios, FuncaoAtual).

executar(5, TabelaHashCupom, Funcionarios, FuncaoAtual) :- 
    menu(TabelaHashCupom, Funcionarios, FuncaoAtual).

executar(6, TabelaHashCupom, Funcionarios, FuncaoAtual) :- 
    menu(TabelaHashCupom, Funcionarios, FuncaoAtual).

executar(7, TabelaHashCupom, Funcionarios, FuncaoAtual) :- 
    menu(TabelaHashCupom, Funcionarios, FuncaoAtual).

executar(8, TabelaHashCupom, Funcionarios, FuncaoAtual) :- 
    (FuncaoAtual = gerente ->
        write('Digite a porcentagem de desconto: '), read(Desconto),
        gerar_cupom(TabelaHashCupom, Desconto, NovaTabelaHash),  
        menu(NovaTabelaHash, Funcionarios, FuncaoAtual)
    ; 
        write('Acesso negado. Somente gerentes podem criar cupons.'), nl,
        menu(TabelaHashCupom, Funcionarios, FuncaoAtual)
    ).

executar(9, TabelaHashCupom, Funcionarios, FuncaoAtual) :- 
    write('Digite o código do cupom: '), read(CupomInput),
    atom_number(CupomAtom, CupomInput),
    verificar_cupom(CupomAtom, TabelaHashCupom),
    menu(TabelaHashCupom, Funcionarios, FuncaoAtual).

executar(10, TabelaHashCupom, Funcionarios, FuncaoAtual) :- 
    write('Digite o código do cupom: '), read(CupomInput),
    atom_number(CupomAtom, CupomInput),
    remover_cupom(CupomAtom, TabelaHashCupom, NovaTabelaHash, Desconto),
    write('Desconto aplicado: '), write(Desconto), write('%'), nl,
    menu(NovaTabelaHash, Funcionarios, FuncaoAtual).

executar(11, _, _, _) :- 
    write('Obrigado por usar o sistema. Até logo!'), nl,
    halt.

executar(_, TabelaHashCupom, Funcionarios, FuncaoAtual) :- 
    write('Opção inválida! Tente novamente.'), nl,
    menu(TabelaHashCupom, Funcionarios, FuncaoAtual).