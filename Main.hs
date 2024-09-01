module Main where

import GeradorDeCupons (HashTable, criarTabelaHashVazia)
import Menu (exibirMenu)
import Login (Funcionario)
import Data.IORef (newIORef)


-- ghc -package containers -o programa Main.hs para compilar o programa
-- .\programa.exe para rodar o programa

main :: IO ()
main = do
    -- Criar referências para os dados
    hashCodigoCupom <- criarTabelaHashVazia
    refEstoque <- newIORef []
    refFuncionarios <- newIORef []  -- Criar referência para os funcionários

    putStrLn "Bem-vindo à caixa registradora P.L.P.!"

    -- Passar todos os parâmetros necessários para exibirMenu
    exibirMenu refEstoque refFuncionarios hashCodigoCupom Nothing
  
