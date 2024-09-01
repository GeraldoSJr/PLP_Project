module Menu (exibirMenu) where

import GeradorDeCupons (HashTable)
import Login (criarLogin, efetuarLogin, Funcao(..), Funcionario)
import Item (Item(..), salvarEstoque, carregarEstoque, adicionarItem, updateItem, deleteItem, listItems, getEstoque, setEstoque, getPreco, setPreco, readItem)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Control.Monad (when)
import Text.Read (readMaybe)

-- Função auxiliar para exibir mensagens e obter input
obterInput :: String -> IO String
obterInput mensagem = do
    putStrLn mensagem
    getLine

-- Função auxiliar para converter input para Int
lerInt :: String -> Maybe Int
lerInt = readMaybe

-- Função auxiliar para converter input para Double
lerDouble :: String -> Maybe Double
lerDouble = readMaybe

-- Função principal para exibir o menu
exibirMenu :: IORef [Item] -> IORef [Funcionario] -> IORef HashTable -> Maybe Funcao -> IO ()
exibirMenu refEstoque refFuncionarios hashCodigoCupom funcaoAtual = do
    putStrLn "Por favor escolha uma das opções abaixo:"
    putStrLn "1. Criar login de acesso"
    putStrLn "2. Efetuar login"
    putStrLn "3. Criar produto"
    putStrLn "4. Ler produto"
    putStrLn "5. Atualizar produto"
    putStrLn "6. Deletar produto"
    putStrLn "7. Gerar relatório"
    putStrLn "Sair"

    input <- getLine

    when (input == "Sair") $ do
        putStrLn "Fechando programa..."
        return ()

    case lerInt input of
        Just 1 -> do
            putStrLn "Digite o nome de usuário:"
            usuario <- getLine
            putStrLn "Digite a senha:"
            senha <- getLine
            putStrLn "Escolha a função (1 para Gerente, 2 para Caixa):"
            funcaoInput <- getLine
            let funcao = if funcaoInput == "1" then Gerente else Caixa
            criarLogin refFuncionarios usuario senha funcao
            putStrLn "Login criado com sucesso!"
            exibirMenu refEstoque refFuncionarios hashCodigoCupom funcaoAtual

        Just 2 -> do
            putStrLn "Digite o nome de usuário:"
            usuario <- getLine
            putStrLn "Digite a senha:"
            senha <- getLine
            funcao <- efetuarLogin refFuncionarios usuario senha
            case funcao of
                Just Gerente -> do
                    putStrLn "Bem-vindo, Gerente!"
                    exibirMenu refEstoque refFuncionarios hashCodigoCupom funcao
                Just Caixa   -> do
                    putStrLn "Bem-vindo, Caixa!"
                    exibirMenu refEstoque refFuncionarios hashCodigoCupom funcao
                Nothing      -> do
                    putStrLn "Login falhou! Verifique suas credenciais."
                    exibirMenu refEstoque refFuncionarios hashCodigoCupom funcaoAtual


        Just 3 -> when (funcaoAtual == Just Gerente) $ do
            nome <- obterInput "Digite o nome do produto:"
            estoqueInput <- obterInput "Digite a quantidade em estoque:"
            precoInput <- obterInput "Digite o preço do produto:"
            case (lerInt estoqueInput, lerDouble precoInput) of
                (Just estoque, Just preco) -> do
                    let novoProduto = Item { itemId = 1, itemNome = nome, itemEstoque = estoque, itemPreco = preco } -- Gerar ID dinamicamente
                    itens <- readIORef refEstoque
                    let itensAtualizados = adicionarItem novoProduto itens
                    writeIORef refEstoque itensAtualizados
                    salvarEstoque itensAtualizados
                    putStrLn "Produto criado com sucesso!"
                _ -> putStrLn "Entrada inválida. Tente novamente."
            exibirMenu refEstoque refFuncionarios hashCodigoCupom funcaoAtual


        Just 4 -> do
            idInput <- obterInput "Digite o ID do produto para ler:"
            case lerInt idInput of
                Just id -> do
                    itens <- readIORef refEstoque
                    case readItem itens id of
                        Just item -> putStrLn $ "Produto: " ++ show item
                        Nothing -> putStrLn "Produto não encontrado."
                _ -> putStrLn "ID inválido. Tente novamente."
            exibirMenu refEstoque refFuncionarios hashCodigoCupom funcaoAtual


        Just 5 -> when (funcaoAtual == Just Gerente) $ do
            idInput <- obterInput "Digite o ID do produto para atualizar:"
            nome <- obterInput "Digite o novo nome do produto:"
            estoqueInput <- obterInput "Digite a nova quantidade em estoque:"
            precoInput <- obterInput "Digite o novo preço do produto:"
            case (lerInt idInput, lerInt estoqueInput, lerDouble precoInput) of
                (Just id, Just estoque, Just preco) -> do
                    itens <- readIORef refEstoque
                    case readItem itens id of
                        Just item -> do
                            let itemAtualizado = item { itemNome = nome, itemEstoque = estoque, itemPreco = preco }
                            itensAtualizados <- updateItem itens itemAtualizado
                            writeIORef refEstoque itensAtualizados
                            salvarEstoque itensAtualizados
                            putStrLn "Produto atualizado com sucesso!"
                        Nothing -> putStrLn "Produto não encontrado."
                _ -> putStrLn "Entrada inválida. Tente novamente."
            exibirMenu refEstoque refFuncionarios hashCodigoCupom funcaoAtual



        Just 6 -> when (funcaoAtual == Just Gerente) $ do
            idInput <- obterInput "Digite o ID do produto para deletar:"
            case lerInt idInput of
                Just id -> do
                    itens <- readIORef refEstoque
                    itensAtualizados <- deleteItem itens id
                    writeIORef refEstoque itensAtualizados
                    salvarEstoque itensAtualizados
                    putStrLn "Produto deletado com sucesso!"
                _ -> putStrLn "ID inválido. Tente novamente."
            exibirMenu refEstoque refFuncionarios hashCodigoCupom funcaoAtual



        Just 7 -> do
            putStrLn "Gerando relatório..."
            putStrLn "não implementada"
            exibirMenu refEstoque refFuncionarios hashCodigoCupom funcaoAtual


        _ -> do
            putStrLn "Opção inválida"
            exibirMenu refEstoque refFuncionarios hashCodigoCupom funcaoAtual


