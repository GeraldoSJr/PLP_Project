module Main where

import GeradorDeCupons
import Menu(exibirMenu)

main :: IO ()
main = do
    hashCodigoCupom <- criarTabelaHashVazia

    putStrLn "Bem-vindo à caixa registradora P.L.P.!"
    
    exibirMenu hashCodigoCupom
  
