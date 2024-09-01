module Login (
    Funcao(..),
    Funcionario(..),
    bancoDeDados,
    criarLogin,
    efetuarLogin
) where

import Data.IORef (IORef, readIORef, modifyIORef, newIORef)
import Control.Monad (when)
import Data.List (find)

-- Define as funções disponíveis
data Funcao = Gerente | Caixa deriving (Show, Eq)

-- Define a estrutura de um funcionário
data Funcionario = Funcionario {
    nomeUsuario :: String,
    senha :: String,
    funcao :: Funcao
} deriving (Show)

-- Pseudo Banco de dados de funcionários (em uma aplicação real, isso seria uma base de dados externa)
bancoDeDados :: IORef [Funcionario] -> IORef [Funcionario]
bancoDeDados refDb = refDb

-- Função para criar um novo login
criarLogin :: IORef [Funcionario] -> String -> String -> Funcao -> IO ()
criarLogin refDb usuario senha funcao = do
    db <- readIORef refDb
    let novoFuncionario = Funcionario usuario senha funcao
    modifyIORef refDb (novoFuncionario :)

-- Função que verifica o login
efetuarLogin :: IORef [Funcionario] -> String -> String -> IO (Maybe Funcao)
efetuarLogin refDb usuario senhaUsuario = do
    db <- readIORef refDb
    let funcionario = find (\f -> nomeUsuario f == usuario && senha f == senhaUsuario) db
    return $ funcao <$> funcionario
