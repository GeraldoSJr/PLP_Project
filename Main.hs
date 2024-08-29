import GeradorDeCupons

main :: IO()
main = do

    hashCodigoCupom <- criarTabelaHashVazia

    putStrLn "Bem vindo a caixa registradora P.L.P.!\nPor favor escolha uma das opções abaixo:"
    putStrLn "1.Criar login de acesso\n2.Efetuar login"
    putStrLn "3.Criar produto\n4.Ler produto\n5.Atualizar produto\n6.Deletar produto"
    putStrLn "7.Gerar relatório"
    putStrLn "Sair"

    input <- getLine

    when(input=="Sair") $ do
        putStrLn "Fechando programa..."
        return()
    
    let escolha = read input :: Int
    
    case escolha of
        1 -> 
        2 ->
        3 ->
        4 ->
        5 ->
        6 ->
        7 ->
        _ -> putStrLn "Opção inválida"

    if escolha `notElem` [1,2,3,4,5,6,7]
        then main
