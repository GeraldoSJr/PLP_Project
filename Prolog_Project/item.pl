% arquivo: item.pl
:- module(item, [
    carregar_estoque/0,
    salvar_estoque/0,
    adicionar_item/3,
    ler_item/2,
    atualizar_item/2,
    atualizar_preco/2,  % Novo predicado para atualização de preço
    deletar_item/1,
    listar_itens/1,
    obter_estoque/2,
    definir_estoque/2,
    obter_preco/2,
    definir_preco/2,
    ler_item_por_nome/2,
    aplicar_desconto/1,
    retirar_desconto/1
]).

:- dynamic item/4.

% Define o caminho do arquivo de estoque
estoque_file('estoque.pl').

%% carregar_estoque
carregar_estoque :-
    estoque_file(File),
    ( exists_file(File) ->
        consult(File)
    ;
        open(File, write, Stream),
        write(Stream, ''),
        close(Stream)
    ).

%% salvar_estoque
salvar_estoque :-
    estoque_file(File),
    findall(item(Id, Nome, Estoque, Preco), item(Id, Nome, Estoque, Preco), Itens),
    open(File, write, Stream),
    forall(member(Item, Itens),
           portray_clause(Stream, Item)),
    close(Stream).

%% gerar_proximo_id(-NovoId)
gerar_proximo_id(NovoId) :-
    ( findall(Id, item(Id, _, _, _), Ids),
      max_list([0|Ids], MaxId) ->
        NovoId is MaxId + 1
    ;
        NovoId = 1
    ).

%% validar_item(+Nome, +Estoque, +Preco)
validar_item(Nome, Estoque, Preco) :-
    atom(Nome),
    Estoque >= 0,
    Preco >= 0.0.

%% adicionar_item(+Nome, +Estoque, +Preco)
adicionar_item(Nome, Estoque, Preco) :-
    validar_item(Nome, Estoque, Preco),
    gerar_proximo_id(Id),
    assertz(item(Id, Nome, Estoque, Preco)),
    salvar_estoque,
    format('Item adicionado com sucesso: ~w~n', [item(Id, Nome, Estoque, Preco)]).

%% ler_item(+Id, -Item)
ler_item(Id, item(Id, Nome, Estoque, Preco)) :-
    item(Id, Nome, Estoque, Preco),
    !.
ler_item(Id, _) :-
    format('Item com ID ~w não encontrado.~n', [Id]),
    fail.

%% atualizar_item(+Id, +ItemAtualizado)
atualizar_item(Id, item(Nome, Estoque, Preco)) :-
    ( retract(item(Id, _, _, _)) ->
        assertz(item(Id, Nome, Estoque, Preco)),
        salvar_estoque,
        format('Item com ID ~w atualizado com sucesso.~n', [Id])
    ;
        format('Item com ID ~w não encontrado.~n', [Id]),
        fail
    ).

%% atualizar_preco(+Id, +NovoPreco)
atualizar_preco(Id, NovoPreco) :-
    definir_preco(Id, NovoPreco).

%% deletar_item(+Id)
deletar_item(Id) :-
    ( retract(item(Id, Nome, Estoque, Preco)) ->
        salvar_estoque,
        format('Item deletado: ~w~n', [item(Id, Nome, Estoque, Preco)])
    ;
        format('Item com ID ~w não encontrado.~n', [Id]),
        fail
    ).

%% listar_itens(-Itens)
listar_itens(Itens) :-
    findall(item(Id, Nome, Estoque, Preco), item(Id, Nome, Estoque, Preco), Itens).

%% obter_estoque(+Id, -Estoque)
obter_estoque(Id, Estoque) :-
    item(Id, _, Estoque, _),
    !.
obter_estoque(Id, _) :-
    format('Item com ID ~w não encontrado.~n', [Id]),
    fail.

%% definir_estoque(+Id, +NovoEstoque)
definir_estoque(Id, NovoEstoque) :-
    NovoEstoque >= 0,
    ( item(Id, Nome, _, Preco) ->
        retract(item(Id, Nome, _, Preco)),
        assertz(item(Id, Nome, NovoEstoque, Preco)),
        salvar_estoque,
        format('Estoque do item ID ~w atualizado para ~w.~n', [Id, NovoEstoque])
    ;
        format('Item com ID ~w não encontrado.~n', [Id]),
        fail
    ).

%% obter_preco(+Id, -Preco)
obter_preco(Id, Preco) :-
    item(Id, _, _, Preco),
    !.
obter_preco(Id, _) :-
    format('Item com ID ~w não encontrado.~n', [Id]),
    fail.

%% definir_preco(+Id, +NovoPreco)
definir_preco(Id, NovoPreco) :-
    NovoPreco >= 0.0,
    ( item(Id, Nome, Estoque, _) ->
        retract(item(Id, Nome, Estoque, _)),
        assertz(item(Id, Nome, Estoque, NovoPreco)),
        salvar_estoque,
        format('Preço do item ID ~w atualizado para ~w.~n', [Id, NovoPreco])
    ;
        format('Item com ID ~w não encontrado.~n', [Id]),
        fail
    ).

%% ler_item_por_nome(+Nome, -Itens)
ler_item_por_nome(Nome, Itens) :-
    findall(item(Id, Nome, Estoque, Preco), item(Id, Nome, Estoque, Preco), Itens),
    ( Itens \= [] ->
        true
    ;
        format('Nenhum item com nome "~w" encontrado.~n', [Nome]),
        fail
    ).

%% aplicar_desconto(+Desconto)
aplicar_desconto(Desconto) :-
    listar_itens(Itens),
    aplicar_desconto_a_itens(Itens, Desconto).

aplicar_desconto_a_itens([], _).
aplicar_desconto_a_itens([item(Id, Nome, Estoque, Preco)|Resto], Desconto) :-
    NovoPreco is Preco * (1 - Desconto / 100),
    atualizar_preco(Id, NovoPreco),
    aplicar_desconto_a_itens(Resto, Desconto).

%% retirar_desconto(+Desconto)
retirar_desconto(Desconto) :-
    listar_itens(Itens),
    retirar_desconto_a_itens(Itens, Desconto).

retirar_desconto_a_itens([], _).
retirar_desconto_a_itens([item(Id, Nome, Estoque, Preco)|Resto], Desconto) :-
    ( Desconto =\= 100 ->
        PrecoOriginal is Preco / (1 - Desconto / 100),
        atualizar_preco(Id, PrecoOriginal)
    ;   
        write('Desconto de 100% não pode ser revertido para o item ID '), write(Id), nl
    ),
    retirar_desconto_a_itens(Resto, Desconto).
