module Item where

import System.IO (withFile, IOMode(..), hPutStrLn, readFile, writeFile)
import System.Directory (doesFileExist)

data Item = Item {
    itemId :: Int,
    itemNome :: String,
    itemEstoque :: Int,
    itemPreco :: Double
} deriving (Show, Read)

-- Função para salvar a lista de itens em "estoque.txt"
salvarEstoque :: [Item] -> IO ()
salvarEstoque itens = do
    let filePath = "estoque.txt"
    -- Garante que o arquivo exista antes de escrever nele
    createFileIfMissing filePath
    withFile filePath WriteMode $ \handle -> do
        mapM_ (hPutStrLn handle . show) itens

-- Função para carregar a lista de itens de "estoque.txt"
carregarEstoque :: IO [Item]
carregarEstoque = do
    let filePath = "estoque.txt"
    fileExists <- doesFileExist filePath
    if not fileExists
        then return []
        else do
            contents <- readFile filePath
            let linhas = lines contents
            return (map read linhas)

-- Função auxiliar para adicionar um item ao estoque
adicionarItem :: Item -> [Item] -> [Item]
adicionarItem novoItem estoque = novoItem : estoque

-- Função auxiliar para criar o arquivo se não existir
createFileIfMissing :: FilePath -> IO ()
createFileIfMissing path = do
    fileExists <- doesFileExist path
    if not fileExists
        then writeFile path ""  -- Cria o arquivo vazio
        else return ()

-- Create: Adiciona um item na lista e salva no estoque.txt
createItem :: [Item] -> Item -> IO [Item]
createItem items newItem = do
    let updatedItems = newItem : items
    salvarEstoque updatedItems
    return updatedItems

-- Read: Encontra um item pelo seu ID
readItem :: [Item] -> Int -> Maybe Item
readItem [] _ = Nothing
readItem (x:xs) searchId
    | itemId x == searchId = Just x
    | otherwise = readItem xs searchId

-- Update: Atualiza um item existente na lista e salva no estoque.txt
updateItem :: [Item] -> Item -> IO [Item]
updateItem [] _ = return []
updateItem (x:xs) updatedItem
    | itemId x == itemId updatedItem = do
        let updatedItems = updatedItem : xs
        salvarEstoque updatedItems
        return updatedItems
    | otherwise = do
        restUpdated <- updateItem xs updatedItem
        let updatedItems = x : restUpdated
        return updatedItems

-- Delete: Remove um item da lista pelo ID e salva no estoque.txt
deleteItem :: [Item] -> Int -> IO [Item]
deleteItem [] _ = return []
deleteItem (x:xs) deleteId
    | itemId x == deleteId = do
        let updatedItems = xs
        salvarEstoque updatedItems
        return updatedItems
    | otherwise = do
        restDeleted <- deleteItem xs deleteId
        let updatedItems = x : restDeleted
        return updatedItems

-- List: Retorna todos os itens
listItems :: [Item] -> [Item]
listItems = id

-- Get: Obtém o estoque de um item pelo seu ID
getEstoque :: [Item] -> Int -> Maybe Int
getEstoque items itemId = itemEstoque <$> readItem items itemId

-- Set: Define o estoque de um item pelo seu ID e salva no estoque.txt
setEstoque :: [Item] -> Int -> Int -> IO [Item]
setEstoque items itemId newEstoque =
    case readItem items itemId of
        Just item -> do
            let updatedItem = item { itemEstoque = newEstoque }
            updatedItems <- updateItem items updatedItem
            salvarEstoque updatedItems -- Salva o estoque atualizado no arquivo
            return updatedItems
        Nothing -> return items -- Ou poderia retornar um erro ou valor padrão se o item não for encontrado

-- Get: Obtém o preço de um item pelo seu ID
getPreco :: [Item] -> Int -> Maybe Double
getPreco items itemId = itemPreco <$> readItem items itemId

-- Set: Define o preço de um item pelo seu ID e salva no estoque.txt
setPreco :: [Item] -> Int -> Double -> IO [Item]
setPreco items itemId newPreco =
    case readItem items itemId of
        Just item -> do
            let updatedItem = item { itemPreco = newPreco }
            updatedItems <- updateItem items updatedItem
            salvarEstoque updatedItems -- Salva o estoque atualizado no arquivo
            return updatedItems
        Nothing -> return items -- Ou poderia retornar um erro ou valor padrão se o item não for encontrado

readItemByName :: [Item] -> String -> Maybe Item
readItemByName [] _ = Nothing
readItemByName (x:xs) nome 
    | itemNome x == nome = Just x
    | otherwise          = readItemByName xs nome