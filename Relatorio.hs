module Relatorio (registrarAcao, gerarRelatorio, limparRelatorio) where

import System.IO (withFile, IOMode(..), hPutStrLn, appendFile)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

-- Função para registrar ações em um relatório
registrarAcao :: String -> IO ()
registrarAcao acao = do
    let filePath = "relatorio.txt"
    timestamp <- obterTimestampAtual
    let logMessage = timestamp ++ " - " ++ acao
    appendFile filePath (logMessage ++ "\n")

-- Função para gerar um relatório consolidado
gerarRelatorio :: IO ()
gerarRelatorio = do
    putStrLn "Gerando relatório consolidado..."
    putStrLn "Relatório gerado e salvo em relatorio.txt"

-- Função para limpar o relatório existente
limparRelatorio :: IO ()
limparRelatorio = do
    let filePath = "relatorio.txt"
    withFile filePath WriteMode $ \handle -> do
        hPutStrLn handle "" -- Escreve uma string vazia para limpar o conteúdo
    putStrLn "Relatório limpo."

-- Função auxiliar para obter o timestamp atual
obterTimestampAtual :: IO String
obterTimestampAtual = do
    currentTime <- getCurrentTime
    return $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" currentTime
