module Menu (exibirMenu) where

import GeradorDeCupons (HashTable)
import Login (criarLogin, efetuarLogin, Funcao(..), bancoDeDados)
import Data.IORef (IORef, newIORef)
import Control.Monad (when)

exibirMenu :: IORef HashTable -> IO ()
exibirMenu hashCodigoCupom = do
    putStrLn "Por favor escolha uma das opções abaixo:"
    putStrLn "1. Criar login de acesso\n2. Efetuar login"
    putStrLn "3. Criar produto\n4. Ler produto\n5. Atualizar produto\n6. Deletar produto"
    putStrLn "7. Gerar relatório"
    putStrLn "Sair"

    input <- getLine

    when (input == "Sair") $ do
        putStrLn "Fechando programa..."
        return()

    let escolha = read input :: Int

    refDb <- newIORef []

    case escolha of
        1 ->  do
            putStrLn "Digite o nome de usuário:"
            usuario <- getLine
            putStrLn "Digite a senha:"
            senha <- getLine
            putStrLn "Escolha a função (1 para Gerente, 2 para Caixa):"
            funcaoInput <- getLine
            let funcao = if funcaoInput == "1" then Gerente else Caixa
            criarLogin refDb usuario senha funcao
            putStrLn "Login criado com sucesso!"
            exibirMenu hashCodigoCupom
        2 -> do
            putStrLn "Digite o nome de usuário:"
            usuario <- getLine
            putStrLn "Digite a senha:"
            senha <- getLine
            funcao <- efetuarLogin refDb usuario senha
            case funcao of
                Just Gerente -> putStrLn "Bem-vindo, Gerente!"
                Just Caixa   -> putStrLn "Bem-vindo, Caixa!"
                Nothing      -> putStrLn "Login falhou! Verifique suas credenciais."
                exibirMenu hashCodigoCupom
        3 -> 
        4 -> 
        5 -> 
        6 -> 
        7 -> 
        _ -> putStrLn "Opção inválida"

    if escolha `notElem` [1,2,3,4,5,6,7]
        then exibirMenu hashCodigoCupom
        else return()
