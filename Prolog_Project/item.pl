:- module(item, [
    carregar_estoque/0,
    salvar_estoque/0,
    adicionar_item/3,
    ler_item/2,
    atualizar_item/2,
    atualizar_preco/2,  % Adicionada nova função
    deletar_item/1,
    listar_itens/1,
    obter_estoque/2,
    definir_estoque/2,
    obter_preco/2,
    definir_preco/2,
    ler_item_por_nome/2
]).


:- dynamic item/4.

% Define o caminho do arquivo de estoque
estoque_file('estoque.pl').

%% carregar_estoque
% Carrega os itens do arquivo estoque.pl para a base de dados dinâmica.
carregar_estoque :-
    estoque_file(File),
    ( exists_file(File) ->
        consult(File)
    ;
        % Se o arquivo não existir, cria um arquivo vazio
        open(File, write, Stream),
        write(Stream, ''),
        close(Stream)
    ).

%% salvar_estoque
% Salva todos os itens atuais na base de dados para o arquivo estoque.pl.
salvar_estoque :-
    estoque_file(File),
    findall(item(Id, Nome, Estoque, Preco), item(Id, Nome, Estoque, Preco), Itens),
    open(File, write, Stream),
    % Escreve cada item como um fato no arquivo
    forall(member(Item, Itens),
           portray_clause(Stream, Item)),
    close(Stream).

%% gerar_proximo_id(-NovoId)
% Gera o próximo ID único para um novo item.
gerar_proximo_id(NovoId) :-
    ( findall(Id, item(Id, _, _, _), Ids),
      max_list([0|Ids], MaxId) ->
        NovoId is MaxId + 1
    ;
        NovoId = 1
    ).

%% validar_item(+Nome, +Estoque, +Preco)
% Valida os dados do item.
validar_item(Nome, Estoque, Preco) :-
    string(Nome),
    Estoque >= 0,
    Preco >= 0.0.

%% adicionar_item(+Nome, +Estoque, +Preco)
% Adiciona um novo item ao estoque.
adicionar_item(Nome, Estoque, Preco) :-
    validar_item(Nome, Estoque, Preco),
    gerar_proximo_id(Id),
    assertz(item(Id, Nome, Estoque, Preco)),
    salvar_estoque,
    format('Item adicionado com sucesso: ~w~n', [item(Id, Nome, Estoque, Preco)]).

%% ler_item(+Id, -Item)
% Lê um item pelo seu ID.
ler_item(Id, item(Id, Nome, Estoque, Preco)) :-
    item(Id, Nome, Estoque, Preco),
    !.
ler_item(Id, _) :-
    format('Item com ID ~w não encontrado.~n', [Id]),
    fail.

%% atualizar_item(+Id, +ItemAtualizado)
% Atualiza um item existente pelo seu ID.
atualizar_item(Id, item(Nome, Estoque, Preco)) :-
    ( retract(item(Id, _, _, _)) ->
        assertz(item(Id, Nome, Estoque, Preco)),
        salvar_estoque,
        format('Item com ID ~w atualizado com sucesso.~n', [Id])
    ;
        format('Item com ID ~w não encontrado.~n', [Id]),
        fail
    ).

%% deletar_item(+Id)
% Deleta um item pelo seu ID.
deletar_item(Id) :-
    ( retract(item(Id, Nome, Estoque, Preco)) ->
        salvar_estoque,
        format('Item deletado: ~w~n', [item(Id, Nome, Estoque, Preco)])
    ;
        format('Item com ID ~w não encontrado.~n', [Id]),
        fail
    ).

%% listar_itens(-Itens)
% Lista todos os itens do estoque.
listar_itens(Itens) :-
    findall(item(Id, Nome, Estoque, Preco), item(Id, Nome, Estoque, Preco), Itens).

%% obter_estoque(+Id, -Estoque)
% Obtém a quantidade em estoque de um item pelo seu ID.
obter_estoque(Id, Estoque) :-
    item(Id, _, Estoque, _),
    !.
obter_estoque(Id, _) :-
    format('Item com ID ~w não encontrado.~n', [Id]),
    fail.

%% definir_estoque(+Id, +NovoEstoque)
% Define a quantidade em estoque de um item pelo seu ID.
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
% Obtém o preço de um item pelo seu ID.
obter_preco(Id, Preco) :-
    item(Id, _, _, Preco),
    !.
obter_preco(Id, _) :-
    format('Item com ID ~w não encontrado.~n', [Id]),
    fail.

%% definir_preco(+Id, +NovoPreco)
% Define o preço de um item pelo seu ID.
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
% Lê todos os itens pelo seu nome.
ler_item_por_nome(Nome, Itens) :-
    findall(item(Id, Nome, Estoque, Preco), item(Id, Nome, Estoque, Preco), Itens),
    ( Itens \= [] ->
        true
    ;
        format('Nenhum item com nome "~w" encontrado.~n', [Nome]),
        fail
    ).
%% atualizar_precos_com_desconto(+Desconto)
% Atualiza o preço de todos os itens no estoque com base no desconto.
atualizar_precos_com_desconto(Desconto) :- 
    findall(Id, item(Id, Nome, Estoque, Preco), Ids),  % Obtém todos os IDs dos itens
    atualizar_precos(Ids, Desconto).

%% atualizar_precos(+Ids, +Desconto)
% Atualiza o preço de cada item com ID em Ids aplicando o desconto.
atualizar_precos([], _).  % Caso base: lista vazia, não faz nada.
atualizar_precos([Id | Resto], Desconto) :- 
    (item(Id, Nome, Estoque, Preco) -> 
        NovoPreco is Preco * (1 - Desconto / 100),  % Calcula o novo preço com desconto
        retract(item(Id, Nome, Estoque, Preco)),
        assertz(item(Id, Nome, Estoque, NovoPreco)),
        format('Preço do item ID ~w atualizado para ~w.~n', [Id, NovoPreco])
    ;
        format('Item com ID ~w não encontrado para atualização de preço.~n', [Id])
    ),
    atualizar_precos(Resto, Desconto).  % Chama recursivamente para os restantes.


