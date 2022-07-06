mostraOpcoes :: Connection -> IO()
mostraOpcoes conn = do
    putStrLn "Bem vindo ao Bestiarius: \
    \\n 1 - Cadastra Pesquisador.\
    \\n 2 - Entrar Pesquisador.\
    \\n 3 - Entrar Visitante.\

    \\n Ao logar como pesquisador: \

	\\n 4 - Cadastrar Criatura.\ 
	\\n 5 - Modificar Criatura.\
	\\n 6 - Listar Minhas Criatura.\

    \\n Ao logar como Visitante.\
    
     \\n 7 - Procura Criaturas.\
	\\n	8 - Listar Criaturas.\
	\\n	9 - Exibir Criatura.\
	\\n	10 - Relatar a Criatura.\
	\\n	11 - Mostrar o útilmo relato da Criatura.\
	\\n	12 - Avistamento de Criatura.\ 
	\\n	13 - Batalha de Criaturas.\
    \\n 14 - Sair."

    inputOpcao <- getLine
    if inputOpcao /= "14" then do
        menu inputOpcao conn
        mostraOpcoes conn
    else
        putStrLn "\nContinue a descobrir os mistérios do mundo, até mais.\n"

main :: IO()
main = do
    conn <- iniciandoDatabase
    putStrLn "Base de dados criada"

    mostraOpcoes conn


menu :: String -> Connection -> IO ()
menu opcao conn
    | opcao == "1" = cadastraPerquisador conn
    | opcao == "2" = entrarPesquisador conn
    | opcao == "3" = entrarVisitante conn
    | opcao == "4" = cadastraCriatura conn
    | opcao == "5" = modificarCriatura conn
    | opcao == "6" = listarMinhasCriaturas conn
    | opcao == "7" = procuraCriatura conn
    | opcao == "8" = listarCriatura conn
    | opcao == "9" = exibirCriatura conn
    | opcao == "10" = relatoCriatura conn
    | opcao == "11" = ultimoRelato conn
    | opcao == "12" = avistamentoCriatura conn
    | opcao == "13" = batalhaCriatura conn
    | otherwise = putStrLn "Opção inválida"

cadastraPesquisador:: Connection -> IO()
cadastroPesquisador conn = do
 putStrLn "\nInicando cadastro de um pesquisador: \n"
    putStrLn "ID > "
    id <- getLine
    putStrLn "Nome > "
    nome <- getLine
    putStrLn "Sobrenome > "
    sobrenome <- getLine
    putStrLn "Senha > "
    senha <- getLine
    cadastraPesquisador conn id nome sobrenome senha