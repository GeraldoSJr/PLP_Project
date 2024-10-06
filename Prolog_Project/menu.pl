:- module(menu, [menu/3]).
:- use_module(geradorDeCupons).
:- use_module(login).
:- use_module(item).
:- use_module(estoque).

menu(TabelaHashCupom, Funcionarios, FuncaoAtual) :- 
    write('--- Menu Principal ---'), nl,
    write('1. Criar login de acesso'), nl,
    write('2. Efetuar login'), nl,
    write('3. Adicionar item'), nl,
    write('4. Ler item'), nl,
    write('5. Atualizar item'), nl,
    write('6. Deletar item'), nl,
    write('7. Listar itens'), nl,
    write('8. Criar cupom desconto'), nl,
    write('9. Verificar cupom de desconto'), nl,
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
    write('Digite o nome do item: '), read(Nome),
    write('Digite a quantidade em estoque: '), read(Estoque),
    write('Digite o preço: '), read(Preco),
    (adicionar_item(Nome, Estoque, Preco) ->
        write('Item adicionado com sucesso.'), nl
    ;
        write('Falha ao adicionar item.'), nl
    ),
    menu(TabelaHashCupom, Funcionarios, FuncaoAtual).

executar(4, TabelaHashCupom, Funcionarios, FuncaoAtual) :- 
    write('Digite o ID do item: '), read(Id),
    (ler_item(Id, Item) ->
        write('Item encontrado: '), write(Item), nl
    ;
        write('Item não encontrado.'), nl
    ),
    menu(TabelaHashCupom, Funcionarios, FuncaoAtual).

executar(5, TabelaHashCupom, Funcionarios, FuncaoAtual) :- 
    write('Digite o ID do item a ser atualizado: '), read(Id),
    write('Digite o novo nome do item: '), read(Nome),
    write('Digite a nova quantidade em estoque: '), read(Estoque),
    write('Digite o novo preço: '), read(Preco),
    (atualizar_item(Id, item(Nome, Estoque, Preco)) ->
        write('Item atualizado com sucesso.'), nl
    ;
        write('Falha ao atualizar item.'), nl
    ),
    menu(TabelaHashCupom, Funcionarios, FuncaoAtual).

executar(6, TabelaHashCupom, Funcionarios, FuncaoAtual) :- 
    write('Digite o ID do item a ser deletado: '), read(Id),
    (deletar_item(Id) ->
        write('Item deletado com sucesso.'), nl
    ;
        write('Falha ao deletar item.'), nl
    ),
    menu(TabelaHashCupom, Funcionarios, FuncaoAtual).

executar(7, TabelaHashCupom, Funcionarios, FuncaoAtual) :- 
    listar_itens(Itens),
    write('Itens disponíveis:'), nl,
    forall(member(Item, Itens), write(Item), nl),
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
    write('Digite o código do cupom: '), 
    read(CupomInput),
    ( atom_number(CupomAtom, CupomInput) -> 
        (remover_cupom(CupomAtom, TabelaHashCupom, NovaTabelaHash, Desconto) ->
            write('Desconto aplicado: '), write(Desconto), write('%'), nl,
            atualizar_precos_com_desconto(Desconto),  % Atualiza todos os preços com o desconto
            write('Todos os preços foram atualizados com desconto de: '), write(Desconto), write('%'), nl
        ;
            write('Código do cupom inválido.'), nl
        )
    ;
        write('Entrada inválida para o código do cupom.'), nl
    ),
    menu(NovaTabelaHash, Funcionarios, FuncaoAtual).


executar(11, _, _, _) :- 
    write('Obrigado por usar o sistema. Até logo!'), nl,
    halt.



executar(_, TabelaHashCupom, Funcionarios, FuncaoAtual) :- 
    write('Opção inválida! Tente novamente.'), nl,
    menu(TabelaHashCupom, Funcionarios, FuncaoAtual).
