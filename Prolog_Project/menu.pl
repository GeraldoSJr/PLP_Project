:- module(menu, [menu/3]).
:- use_module(geradorDeCupons).
:- use_module(login).
:- use_module(item).
:- use_module(estoque).
:- use_module(relatorio).  % Include the relatorio module


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
    write('11. Gerar relatório'), nl,
    write('12. Limpar relatório'), nl,
    write('13. Sair'), nl,
    write('Escolha uma opcao: '),
    read(Opcao),
    executar(Opcao, TabelaHashCupom, Funcionarios, FuncaoAtual).

executar(1, TabelaHashCupom, Funcionarios, FuncaoAtual) :- 
    write('Digite o nome de usuário: '), read(NomeUsuario),
    write('Digite a senha: '), read(Senha),
    write('Digite a função (gerente/caixa): '), read(Funcao),
    (criar_login(NomeUsuario, Senha, Funcao, Funcionarios, NovosFuncionarios) ->
        write('Login criado com sucesso!'), nl,
        registrar_acao('Login criado com sucesso'),  % Log action
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
        registrar_acao('Login efetuado com sucesso'),  % Log action
        menu(TabelaHashCupom, Funcionarios, NovaFuncao)
    ; 
        write('Falha no login. Verifique suas credenciais.'), nl,
        registrar_acao('Falha no login'),  % Log action
        menu(TabelaHashCupom, Funcionarios, none)
    ).

executar(3, TabelaHashCupom, Funcionarios, FuncaoAtual) :- 
    write('Digite o nome do item: '), read(Nome),
    write('Digite a quantidade em estoque: '), read(Estoque),
    write('Digite o preço: '), read(Preco),
    (adicionar_item(Nome, Estoque, Preco) ->
        write('Item adicionado com sucesso.'), nl,
        registrar_acao('Item adicionado'),  % Log action
        menu(TabelaHashCupom, Funcionarios, FuncaoAtual)
    ;
        write('Falha ao adicionar item.'), nl,
        menu(TabelaHashCupom, Funcionarios, FuncaoAtual)
    ).

executar(4, TabelaHashCupom, Funcionarios, FuncaoAtual) :- 
    write('Digite o ID do item: '), read(Id),
    (ler_item(Id, Item) ->
        write('Item encontrado: '), write(Item), nl,
        registrar_acao('Item lido'),  % Log action
        menu(TabelaHashCupom, Funcionarios, FuncaoAtual)
    ;
        write('Item não encontrado.'), nl,
        menu(TabelaHashCupom, Funcionarios, FuncaoAtual)
    ).

executar(5, TabelaHashCupom, Funcionarios, FuncaoAtual) :- 
    write('Digite o ID do item a ser atualizado: '), read(Id),
    write('Digite o novo nome do item: '), read(Nome),
    write('Digite a nova quantidade em estoque: '), read(Estoque),
    write('Digite o novo preço: '), read(Preco),
    (atualizar_item(Id, item(Nome, Estoque, Preco)) ->
        write('Item atualizado com sucesso.'), nl,
        registrar_acao('Item atualizado'),  % Log action
        menu(TabelaHashCupom, Funcionarios, FuncaoAtual)
    ;
        write('Falha ao atualizar item.'), nl,
        menu(TabelaHashCupom, Funcionarios, FuncaoAtual)
    ).

executar(6, TabelaHashCupom, Funcionarios, FuncaoAtual) :- 
    write('Digite o ID do item a ser deletado: '), read(Id),
    (deletar_item(Id) ->
        write('Item deletado com sucesso.'), nl,
        registrar_acao('Item deletado'),  % Log action
        menu(TabelaHashCupom, Funcionarios, FuncaoAtual)
    ;
        write('Falha ao deletar item.'), nl,
        menu(TabelaHashCupom, Funcionarios, FuncaoAtual)
    ).

executar(7, TabelaHashCupom, Funcionarios, UsuarioAtual) :-
    write('Lista de itens:'), nl,
    listar_itens(Itens),
    forall(member(item(Id, Nome, Estoque, Preco), Itens),
            format('ID: ~w, Nome: ~w, Estoque: ~w, Preço: ~2f~n', [Id, Nome, Estoque, Preco])),
    menu(TabelaHashCupom, Funcionarios, UsuarioAtual).

executar(8, TabelaHashCupom, Funcionarios, FuncaoAtual) :- 
    (FuncaoAtual = gerente ->
        write('Digite a porcentagem de desconto: '), read(Desconto),
        gerar_cupom(TabelaHashCupom, Desconto, NovaTabelaHash),  
        registrar_acao('Cupom gerado'),  % Log action
        menu(NovaTabelaHash, Funcionarios, FuncaoAtual)
    ; 
        write('Acesso negado. Somente gerentes podem criar cupons.'), nl,
        menu(TabelaHashCupom, Funcionarios, FuncaoAtual)
    ).

executar(9, TabelaHashCupom, Funcionarios, FuncaoAtual) :- 
    write('Digite o código do cupom: '), read(CupomInput),
    atom_number(CupomAtom, CupomInput),
    verificar_cupom(CupomAtom, TabelaHashCupom),
    registrar_acao('Cupom verificado'),  % Log action
    menu(TabelaHashCupom, Funcionarios, FuncaoAtual).

executar(10, TabelaHashCupom, Funcionarios, FuncaoAtual) :- 
    write('Digite o código do cupom: '),
    read(CupomInput),
    (   integer(CupomInput)
    ->  number_atom(CupomInput, CupomAtom)
    ;   CupomAtom = CupomInput
    ),
    % Buscar o desconto usando get_desconto
    (   geradorDeCupons:get_desconto(CupomAtom, TabelaHashCupom, Desconto)
    ->  write('Desconto encontrado: '), write(Desconto), write('%'), nl,
        write('Digite o ID do item para atualizar o preço: '), read(ItemIDInput),
        (   integer(ItemIDInput)
        ->  ItemID = ItemIDInput
        ;   atom_number(ItemIDInput, ItemID)
        ),
        % Tenta ler o item e atualizar seu preço
        (   item:ler_item(ItemID, item(ItemID, Nome, Estoque, Preco))
        ->  NovoPreco is Preco * (1 - Desconto / 100),
            item:atualizar_item(ItemID, item(Nome, Estoque, NovoPreco)),
            write('Preço do item atualizado para: '), write(NovoPreco), nl,
            registrar_acao('Desconto aplicado ao item ID ' + ItemID + ' com desconto ' + Desconto + '%')
        ;   write('Item não encontrado.'), nl
        )
    ;   write('Código do cupom inválido.'), nl
    ),
    geradorDeCupons:salvar_cupons,
    menu(TabelaHashCupom, Funcionarios, FuncaoAtual).


executar(11, TabelaHashCupom, Funcionarios, FuncaoAtual) :- 
    gerar_relatorio,  % Generate report
    registrar_acao('Relatório gerado'),  % Log action
    menu(TabelaHashCupom, Funcionarios, FuncaoAtual).

executar(12, TabelaHashCupom, Funcionarios, FuncaoAtual) :- 
    limpar_relatorio,  % Clear report
    registrar_acao('Relatório limpo'),  % Log action
    menu(TabelaHashCupom, Funcionarios, FuncaoAtual).

executar(13, _, _, _) :- 
    write('Obrigado por usar o sistema. Até logo!'), nl,
    registrar_acao('Sistema encerrado'),  % Log action
    halt.

executar(_, TabelaHashCupom, Funcionarios, FuncaoAtual) :- 
    write('Opção inválida! Tente novamente.'), nl,
    menu(TabelaHashCupom, Funcionarios, FuncaoAtual).
