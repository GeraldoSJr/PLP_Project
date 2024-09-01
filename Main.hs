module Main where

import GeradorDeCupons
import Menu(exibirMenu)
import Login (bancoDeDados)
import Data.IORef (newIORef)


-- ghc -package containers -o programa Main.hs para compilar o programa
-- .\programa.exe para rodar o programa

main :: IO ()
main = do
    hashCodigoCupom <- criarTabelaHashVazia

    refEstoque <- newIORef []

    putStrLn "Bem-vindo à caixa registradora P.L.P.!"
    
    exibirMenu refEstoque hashCodigoCupom
  
