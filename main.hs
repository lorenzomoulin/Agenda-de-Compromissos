import System.IO
import Data.Char
import Trabalho1

main = do
	agenda <- openFile "agenda.txt" ReadMode
	calendario <- openFile "calendario.txt" ReadMode
	agendaLida <- leAgenda agenda
	calendarioLido <- leCalendario calendario
	hClose agenda
	hClose calendario
	menu agendaLida calendarioLido
	

leAgenda agenda = do
	vazio <- hIsEOF agenda
	if vazio == True then
		do
	 		return ([])
	else
		do 
			agendaLida <- leTuplaMes agenda []
			return (agendaLida)

--função que recebe handle agenda e adiciona uma tupla na agenda caso ainda haja no arquivo, caso contrário retorna a lista
leTuplaMes agendaArq lista = do
	eof <- hIsEOF agendaArq
	if eof == True then
		do
			return lista
	else
		do
			tuplaMes <- leTuplaDia agendaArq
			leTuplaMes agendaArq (lista ++ [tuplaMes])


leTuplaDia agendaArq = do
	 mes <- hGetLine agendaArq
	 listaDias <- leListaDia agendaArq []
	 return ((read mes :: Int, listaDias))

--funcao que lê as tuplas de cada dia e retorna a lista de compromissos de um mês, caso seja o final do arquivo a função retorna a listaMes
--caso contrário, se a próxima linha lida for nula, significa que o mes acabou entao retorna-se a listaMes
--caso contrário, lê-se a linha quem contém a lista de compromissos, então concatena listaMes com a tupla lida
leListaDia agendaArq listaMes = do
	eof <- hIsEOF agendaArq
	if eof == True then
		do
			return listaMes
	else
		do
			dia <- hGetLine agendaArq
			if dia == "" then
				do
					return listaMes
			else
				do
					listaCompromissos <- hGetLine agendaArq
					leListaDia agendaArq (listaMes ++ [(read dia :: Int, read listaCompromissos :: [(Int,Int)])])


-- função que lê calendrio, primeira linha é Bool, as próximas serão lidas pela função leNaoUteis
-- retorna (Bool, [(Int,Int)])
leCalendario calendarioArq = do
	bissexto <- hGetLine calendarioArq

	listaNaoUteis <- leNaoUteis calendarioArq [] 1


	return $ (read bissexto :: Bool, listaNaoUteis )

-- le as linhas restantes do arquivo calendario.txt, a linha é convertida de String para [(Int, Int)] através da função converteParaListaTuplas
leNaoUteis calendarioArq listaNaoUteis mes = do
	eof <- hIsEOF calendarioArq
	if eof == True then
		do
			return listaNaoUteis
	else
		do
			naoUteisMes <- hGetLine calendarioArq
			leNaoUteis calendarioArq (listaNaoUteis ++ (listaTuplaNaoUteis (words naoUteisMes) mes)) (mes + 1)

listaTuplaNaoUteis ls mes = [(mes, read x :: Int) | x <- ls]	

-- compreensão de lista que associa o primeiro elemento de uma lista com os restantes
converteParaListaTuplas str = [(read (str !! 0) :: Int, read (str !! x) :: Int) | x <- [1..((length str) - 1)]]

escreveAgenda agendaArq []  = do
	return "ok"

escreveAgenda agendaArq agenda  = do
	
	escreveMes agendaArq (head agenda)
	escreveAgenda agendaArq (tail agenda)


escreveMes agendaArq tupla = do
	
	hPutStrLn agendaArq (show (fst(tupla)))
	escreveDias agendaArq (snd(tupla))
	hPutStrLn agendaArq ""

escreveDias agendaArq [] = do
	return "ok"

escreveDias agendaArq ls = do
	escreveDia agendaArq (head ls)
	escreveDias agendaArq (tail ls)

escreveDia agendaArq tupla = do
	hPutStrLn agendaArq (show (fst(tupla)))
	hPutStrLn agendaArq (show (snd(tupla)))

imprimeMenu = do 
	putStrLn "0 - Sair"
	putStrLn "1 - Recuperar Agenda"
	putStrLn "2 - Verificar disponibilidade de horário"
	putStrLn "3 - Verificar disponibilidade no dia"
	putStrLn "4 - Inserir compromisso no horário"
	putStrLn "5 - Inserir compromisso mais breve"
	putStrLn "6 - Inserir compromisso no intervalo mínimo"
	putStrLn "7 - Inserir compromisso no intervalo máximo"
	putStrLn "8 - Cancelar compromisso"
	putStrLn "9 - Reagendar compromisso"
	putStrLn "10 - Gravar agenda"

trataMenu opcao agenda calendario = do
	if opcao == "0" then
		do
			putStrLn "Sair"
			return "Sair"

	else if opcao == "1" then
		do 
			
			putStrLn (show agenda)
			
			return (show agenda)

	else if opcao == "2" then
		do
			putStr "Digite o mês: "
			mes <- getLine
			putStr "Digite o dia: "
			dia <- getLine
			putStr "Digite o horário de início: "
			inicio <- getLine
			putStr "Digite a duração: "
			duracao <- getLine
			putStrLn (show (disponibilidade agenda calendario (read mes :: Int) (read dia :: Int) (read inicio :: Int) (read duracao :: Int)))			
			return (show agenda)
		
	else if opcao == "3" then
		do
			putStr "Digite o mês: "
			mes <- getLine
			putStr "Digite o dia: "
			dia <- getLine
			putStrLn (show (horariosDisponiveis agenda calendario (read mes :: Int) (read dia :: Int)))
			return (show agenda)
	
	else if opcao == "4" then
		do
			putStr "Digite o mês: "
			mes <- getLine
			putStr "Digite o dia: "
			dia <- getLine
			putStr "Digite o horário de início: "
			inicio <- getLine
			putStr "Digite a duração: "
			duracao <- getLine
			putStrLn (show (insere agenda calendario (read mes :: Int) (read dia :: Int) (read inicio :: Int) (read duracao :: Int)))
			return (show (insere agenda calendario (read mes :: Int) (read dia :: Int) (read inicio :: Int) (read duracao :: Int)))
	else if opcao == "5" then
		do
			putStr "Digite o mês: "
			mes <- getLine
			putStr "Digite o dia: "
			dia <- getLine
			putStr "Digite a duração: "
			duracao <- getLine
			putStrLn (show (insereBreve agenda calendario (read mes :: Int) (read dia :: Int) (read duracao :: Int)))
			return (show (insereBreve agenda calendario (read mes :: Int) (read dia :: Int) (read duracao :: Int)))	

	else if opcao == "6" then
		do
			putStr "Digite o mês: "
			mes <- getLine
			putStr "Digite a duração: "
			duracao <- getLine
			putStrLn (show (insereMin agenda calendario (read mes :: Int) (read duracao :: Int)))
			return (show (insereMin agenda calendario (read mes :: Int) (read duracao :: Int)))	
			
	else if opcao == "7" then
		do 
			putStr "Digite o mês: "
			mes <- getLine
			putStr "Digite a duração: "
			duracao <- getLine
			putStrLn (show (insereMax agenda calendario (read mes :: Int) (read duracao :: Int)))
			return (show (insereMax agenda calendario (read mes :: Int) (read duracao :: Int)))

	else if opcao == "8" then
		do
			putStr "Digite o mês: "
			mes <- getLine
			putStr "Digite o dia: "
			dia <- getLine
			putStr "Digite o horário: "
			horario <- getLine
			putStrLn (show (cancela agenda (read mes :: Int) (read dia :: Int) (read horario :: Int)))
			return (show (cancela agenda (read mes :: Int) (read dia :: Int) (read horario :: Int)))

	else if opcao == "9" then
		do
			putStr "Digite o mês: " 
			mes <- getLine
			putStr "Digite o dia: "
			dia <- getLine
			putStr "Digite o horário de início do compromisso a ser reagendado: "
			horarioInicio <- getLine
			putStr "Digite o novo mês: "
			novoMes <- getLine
			putStr "Digite o novo dia: "
			novoDia <- getLine
			putStr "Digite o novo horário de início: "
			novoHorarioInicio <- getLine
			putStr "Digite a nova duração: "
			novaDuracao <- getLine
			putStrLn (show (reagenda agenda calendario (read mes :: Int) (read dia :: Int) (read horarioInicio :: Int) (read novoMes :: Int) (read novoDia :: Int) (read novoHorarioInicio :: Int) (read novaDuracao :: Int)))
			return (show (reagenda agenda calendario (read mes :: Int) (read dia :: Int) (read horarioInicio :: Int) (read novoMes :: Int) (read novoDia :: Int) (read novoHorarioInicio :: Int) (read novaDuracao :: Int)))

	else if opcao == "10" then
		do
			agendaArq <- openFile "agenda.txt" WriteMode
			
			
			escreveAgenda agendaArq (agenda)
			hClose agendaArq
			return (show agenda)
			
	else	
		do 
			putStrLn "Opção inválida!"
			return "Opção inválida!"


menu agenda calendario = do
	imprimeMenu
	opcao <- getLine
	resultado <- trataMenu opcao agenda calendario
	if  resultado == "Sair" then
		do
			return ("fim")
	else
		do
			menu (read resultado :: CsAno) calendario
