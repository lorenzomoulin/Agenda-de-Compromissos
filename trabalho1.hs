--notacao: c = compromissos, cs = compromissos
--definindo um compromisso
type C = (Int, Int)

--definindo os compromissos de um dia
type CsDia = (Int, [C])

--definindo os compromissos de um mês
type CsMes = (Int, [CsDia])

--definindo os compromissos de um ano
type CsAno = [CsMes]

--definindo o calendário
type Calendario = (Bool, [(Int, Int)])

--comparador para ordenar compromissos
comparaTupla c1 c2 = fst(c1) <= fst(c2)

--ordena uma lista de compromissos do dia
ordenaCsDia [] = []
ordenaCsDia (l:ls) = ordenaCsDia [x | x <- ls, comparaTupla x l] ++ [l] ++ ordenaCsDia [x | x <- ls, comparaTupla l x]

--ordena lista de compromissos do mes
ordenaCsMes [] = []
ordenaCsMes (l:ls) = ordenaCsMes [x | x <- ls, comparaTupla x l] ++ [l] ++ ordenaCsMes [y | y <- ls, comparaTupla l y]

csMes3 = (3, [ (5, [(8,1), (10,1), (15,2)]), (2, [ (9,2), (14,1)]) ])

--retorna True se o compromisso estiver na lista de compromissos e False caso contrario
buscaC ano mes dia horario = [h | m <- ano, fst(m) == mes, d <- snd(m), fst(d) == dia, h <- snd(d), fst(h) == horario || (fst(h) + snd(h)) > horario]
verificaNaAgenda ano mes dia horario = buscaC ano mes dia horario /= []
ano = [(1,[(2,[(8,2)]),(3,[(8,1), (9,1)])]),(2,[(4,[(10,1)])])]
listaDeDia = [(8,1), (9,2), (11,3)]
--buscaC (c:cs) mes dia horario = if fst(c) == mes && 

--horaFinal de um compromisso
horaFinal c = if fst(c) + snd(c) > 12 && fst(c) < 14 
				then 14 + snd(c) - (12 - fst(c)) 
				else fst(c) + snd(c)

--procurar elemento em uma lista, input: ls -> lista, ele -> elemento
procuraElemento ls ele = [e | e <- ls, e == ele] /= []

--retornar lista de dias de um mes com os compromissos de cada mes
--input: agenda de compromissos, mes
--output: lista de dias
retornaListaDias ag mes = head [snd(x) | x <- ag, fst(x) == mes]

--retornar compromissos de um dia
--input: lista de dias de um mes, dia
--output: lista dos compromissos do dia
retornaCDia ldms dia = head [snd(x) | x <- ldms, fst(x) == dia]

podeInserir cInserir ls = if ls == [] 
							then True 
							else [c | c <- ls, conflito c cInserir == True] == []

--funcao que insere um compromisso em uma lista de compromissos de um dia
insereCNaLista lsD comp = [esq | esq <- lsD, fst(esq) < fst(comp)] ++ [comp] ++ [dir | dir <- lsD, fst(dir) > fst(comp)] 

--funcao para modificar a tupla de um mes
modificaMes tuplaMes dia compromisso = (fst(tuplaMes), [if fst(d) == dia 
															then (dia, (insereCNaLista (snd(d)) compromisso)) 
															else d | d <- snd(tuplaMes)])

--funcao que retira eloemento de uma lista
retiraCDaLista lsD horario = [esq | esq <- lsD, fst(esq) < horario] ++ [dir | dir <- lsD, fst(dir) > horario]

--funcao para modificar a tupla de um mes
retiraDoMes tuplaMes dia horario = (fst(tuplaMes), [if fst(d) == dia 
														then (dia, (retiraCDaLista (snd(d)) horario)) 
														else d | d <- snd(tuplaMes)])

--funcao que procura um compromisso dado o horario de inicio
procuraTupla ls ele = [e | e <- ls, fst(e) == ele] /= []

--funcao para ver se um compromisso conflita com outro
conflito cConfirmado cInserir = if fst(cConfirmado) == fst(cInserir) 
									then True 
									else if (fst(cConfirmado) < fst(cInserir)) 
										then fst(cInserir) < horaFinal(cConfirmado) 
										else horaFinal(cInserir) > fst(cConfirmado)

--adiciona o primeiro compromisso de um mes
adicionaTuplaMes ag mes dia horario duracao = [esq | esq <- ag, fst(esq) < mes] ++ 
												[(mes, [(dia, [(horario, duracao)])])] ++ 
												[dir | dir <- ag, fst(dir) > mes]

--adiciona o primeiro compromisso de um dia
adicionaTuplaDia ls mes dia horario duracao = (mes, [esq | esq <- ls, fst(esq) < dia] ++ 
												[(dia, [(horario, duracao)])] ++ 
												[dir | dir <- ls, fst(dir) > dia])

--inserir compromisso em um dia de um mes
--input: agenda, calendario, mes, dia, horario inicio, duracao
--output: agenda atualizada
insere ag cal mes dia horario duracao  | procuraElemento (snd(cal)) (mes, dia)  == True = ag
									   | (fst(cal) == False && dia == 29 && mes == 2) = ag
									   | (horario < 8 || (horario >= 12 && horario < 14) || horaFinal (horario, duracao) > 18 || horario >= 18) = ag
									   | procuraTupla ag mes == False = adicionaTuplaMes ag mes dia horario duracao
									   | procuraTupla (retornaListaDias ag mes) dia == False = [if fst(m) == mes 
									   																then adicionaTuplaDia (retornaListaDias ag mes) mes dia horario duracao 
									   																else m | m <- ag]
									   | not (podeInserir (horario, duracao) (retornaCDia (retornaListaDias ag mes) dia)) = ag
									   | otherwise = [if fst(m) == mes 
									   					then modificaMes m dia (horario, duracao) 
									   					else m | m <- ag]
									   
cancela ag mes dia horario | procuraTupla (retornaCDia (retornaListaDias ag mes) dia) horario == False = ag
						   | otherwise = [if fst(m) == mes 
						   					then retiraDoMes m dia horario 
						   					else m | m <- ag]

reagenda ag cal mes dia horarioInicio novoMes novoDia novoHorario novaDuracao | insere ag cal novoMes novoDia novoHorario novaDuracao == ag = ag
																			  | otherwise = insere (cancela ag mes dia horarioInicio) cal novoMes novoDia novoHorario novaDuracao

disponibilidade ag cal mes dia horario duracao | procuraElemento (snd(cal)) (mes, dia)  == True = False
									   		   | (fst(cal) == False && dia == 29 && mes == 2) = False
									    	   | (horario < 8 || (horario >= 12 && horario < 14) || horaFinal (horario, duracao) > 18 || horario >= 18) = False
									    	   | procuraTupla ag mes == False = True
									    	   | procuraTupla (retornaListaDias ag mes) dia == False = True
									    	   | not (podeInserir (horario, duracao) (retornaCDia (retornaListaDias ag mes) dia)) = True
									    	   | otherwise = True

listaOcupados ls = [z | x <- ls, z <- [fst(x)..(horaFinal(x)-1)]]
listaDisponiveis ls = [d | d <- [8..17], not(procuraElemento ls d) && d /= 12 && d /= 13]

horariosDisponiveis ag cal mes dia | procuraElemento (snd(cal)) (mes, dia)  == True = []
								   | (fst(cal) == False && dia == 29 && mes == 2) = []
								   | otherwise = listaDisponiveis (listaOcupados(retornaCDia (retornaListaDias ag mes) dia))

ultimoDiaMes cal mes | mes == 4 || mes == 6 || mes == 9 || mes == 11 = 30
				 	 | mes == 2 && fst(cal) == True = 29
				 	 | mes == 2 && fst(cal) == False = 28
				 	 | otherwise = 31

--retorna a primeira aparição de um elemento diferente de (0,0)
procuraElementoDiferente ls = encaixaComp [x | x <- ls, x /= (0,0)]

--retorna a primeira aparição de um elemento diferente de 0
procuraDiaDiferente ls =  encaixaDia [x | x <- ls, x /= 0]

--retorna o primeiro dia disponivel
encaixaDia [] = 0
encaixaDia (x:xs) = x

--retorna o primeiro compromisso disponivel
encaixaComp [] = (0,0)
encaixaComp (x:xs) = x

--retorna (0,0) se nao for possivel inserir um compromisso na lista do dia
tentaEncaixarDia ls duracao = encaixaComp [(x, duracao) | x <- [8,9,10,11,14,15,16,17], podeInserir (x,duracao)ls && horaFinal (x, duracao) <= 18] 

--retorna 
percorreMes cal ls mes dia duracao = procuraElementoDiferente [if procuraTupla ls x == False 
																then (8,duracao)
																else tentaEncaixarDia (retornaCDia ls x) duracao | x <- [dia..(ultimoDiaMes cal mes)], procuraElemento (snd(cal)) (mes, dia) == False] 

retornaDiaBreve cal ls mes dia duracao = procuraDiaDiferente [if procuraTupla ls x == False 
																then x
																else if tentaEncaixarDia (retornaCDia ls x) duracao /= (0,0)
																then x
																else 0 | x <- [dia..(ultimoDiaMes cal mes)], procuraElemento (snd(cal)) (mes, dia) == False] 


insereBreve ag cal mes dia duracao | mes == 13 = ag
								   | procuraElemento (snd(cal)) (mes, dia)  == True = ag
								   | (fst(cal) == False && dia == 29 && mes == 2) = ag
								   | procuraTupla ag mes == False && horaFinal(8,duracao) <= 18 = adicionaTuplaMes ag mes dia 8 duracao
								   | procuraTupla (retornaListaDias ag mes) dia == False  && horaFinal(8,duracao) <= 18 = [if fst(m) == mes 
								   																							then adicionaTuplaDia (retornaListaDias ag mes) mes dia 8 duracao 
								   																							else m | m <- ag]
								   | percorreMes cal (retornaListaDias ag mes) mes dia duracao == (0,0) = insereBreve ag cal (mes + 1) dia duracao
								   | otherwise = [if procuraTupla (retornaListaDias ag mes) (retornaDiaBreve cal (retornaListaDias ag mes) mes dia duracao) == False 
								   						then  adicionaTuplaDia (retornaListaDias ag mes) mes (retornaDiaBreve cal (retornaListaDias ag mes) mes dia duracao) 8 duracao 		
									   					else if fst(m) == mes
									   						then modificaMes m  (retornaDiaBreve cal (retornaListaDias ag mes) mes dia duracao) (percorreMes cal (retornaListaDias ag mes) mes dia duracao) 
									   						else m | m <- ag]




								   	{-[if procuraTupla (retornaListaDias ag mes) dia == False 
								   						then  adicionaTuplaDia (retornaListaDias ag mes) mes dia 8 duracao 		
									   					else if fst(m) == mes
									   						then modificaMes m dia (percorreMes cal (retornaListaDias ag mes) mes dia duracao) 
									   						else m | m <- ag]-}