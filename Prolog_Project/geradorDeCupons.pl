- module(geradorDeCupons, [gerar_cupom/3, verificar_cupom/2, remover_cupom/4, aplicar_desconto/4, retirar_desconto/4]).
:- use_module(library(assoc)).
:- use_module(estoque).  % Adicione esta linha para usar funções do estoque.pl

% Gera um novo cupom com um desconto especificado e atualiza a tabela de cupons.
gerar_cupom(TabelaHashAtual, Desconto, TabelaFinal) :- 
    gerar_codigo_cupom(Cupom),
    (   get_assoc(Cupom, TabelaHashAtual, _) 
    ->  gerar_cupom(TabelaHashAtual, Desconto, TabelaFinal)  % Chamada recursiva se o cupom já existir
    ;   put_assoc(Cupom, TabelaHashAtual, Desconto, TabelaFinal),
        write('Novo cupom gerado: '), write(Cupom), 
        write(', Desconto: '), write(Desconto), write('%'), nl
    ).

% Gera um código de cupom aleatório entre 1000 e 9999.
gerar_codigo_cupom(Cupom) :- 
    random(1000, 9999, Numero), 
    atom_number(Cupom, Numero). 

% Verifica se um cupom existe e exibe seu desconto.
verificar_cupom(Cupom, TabelaHash) :- 
    (   get_assoc(Cupom, TabelaHash, Desconto) 
    ->  format('Cupom ~w encontrado com desconto: ~w%~n', [Cupom, Desconto])
    ;   write('Cupom não encontrado.'), nl
    ).

% Remove um cupom da tabela hash e retorna a nova tabela.
remover_cupom(Cupom, TabelaHashAtual, TabelaHashAtualNova, Desconto) :- 
    (   get_assoc(Cupom, TabelaHashAtual, Desconto)  % Obtém o desconto antes de remover
    ->  del_assoc(Cupom, TabelaHashAtual, TabelaHashAtualNova, _)  % Remove o cupom
    ;   TabelaHashAtualNova = TabelaHashAtual, Desconto = none  % Caso o cupom não exista
    ).

% Aplica um desconto do cupom no estoque atual e gera um novo estoque.
aplicar_desconto(Cupom, EstoqueAtual, EstoqueNovo, TabelaHash) :- 
    (   get_assoc(Cupom, TabelaHash, Desconto) 
    ->  aplicar_desconto_no_estoque(Desconto, EstoqueAtual, EstoqueNovo)
    ;   write('Cupom não encontrado. Não foi possível aplicar o desconto.'), nl,
        EstoqueNovo = EstoqueAtual
    ).

% Aplica o desconto a cada item do estoque.
aplicar_desconto_no_estoque(Desconto, EstoqueAtual, EstoqueNovo) :- 
    findall(ItemNovo, (
        member(Item, EstoqueAtual),
        Item = item(ID, Nome, Preco, Estoque),
        PrecoNovo is Preco * (1 - Desconto / 100),
        ItemNovo = item(ID, Nome, PrecoNovo, Estoque)
    ), EstoqueNovo).

% Retira o desconto aplicado nos itens do estoque.
retirar_desconto(Cupom, EstoqueAtual, EstoqueNovo, TabelaHash) :- 
    (   get_assoc(Cupom, TabelaHash, Desconto)
    ->  findall(ItemNovo, (
            member(Item, EstoqueAtual),
            Item = item(ID, Nome, Preco, Estoque),
            PrecoNovo is Preco / (1 - (Desconto / 100)),  % Corrigido para usar o desconto apropriado
            ItemNovo = item(ID, Nome, PrecoNovo, Estoque)
        ), EstoqueNovo)
    ;   write('Cupom não encontrado. Não foi possível retirar o desconto.'), nl,
        EstoqueNovo = EstoqueAtual
    ).

