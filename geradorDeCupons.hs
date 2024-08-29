
module GeradorDeCupons (
    HashTable,
    criarTabelaHashVazia,
    adicionarStringAleatoria,
    buscarValorAssociado
) where

import qualified Data.Map as Map
import Data.IORef (IORef, newIORef, readIORef, modifyIORef)
import System.Random (randomRIO)
import Data.Maybe (fromMaybe)

type HashTable = Map.Map String [(String, Double)]

-- Função para criar uma nova tabela hash vazia
criarTabelaHashVazia :: IO (IORef HashTable)
criarTabelaHashVazia = newIORef Map.empty

-- Função para gerar uma string aleatória de um comprimento específico
gerarStringAleatoria :: Int -> IO String
gerarStringAleatoria length = sequence $ replicate length (randomRIO ('a', 'z'))

-- Função que adiciona uma string aleatória à lista associada a uma chave
adicionarStringAleatoria :: IORef HashTable -> String -> Double -> IO ()
adicionarStringAleatoria refHashTable chave valor = do
    randomStr <- gerarStringAleatoria 8
    modifyIORef refHashTable $ \tabelaHash ->
        let listaAtualizada = case Map.lookup chave tabelaHash of
                                Just listaAntiga -> (randomStr,valor) : listaAntiga
                                Nothing -> [(randomStr,valor)]
        in Map.insert chave listaAtualizada tabelaHash

-- Função que busca o valor associado a uma string aleatória
buscarValorAssociado :: IORef HashTable -> String -> IO (Maybe Double)
buscarValorAssociado refHashTable randomStr = do
    tabelaHash <- readIORef refHashTable
    let listas = concat $ Map.elems tabelaHash
    return $ lookup randomStr listas