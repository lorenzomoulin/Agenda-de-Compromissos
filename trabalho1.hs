--notacao: c = compromissos, cs = compromissos
--definindo um compromisso
type C = (Int, Int)

--definindo os compromissos de um dia
type CsDia = (Int, [C])

--definindo os compromissos de um mês
type CsMes = (Int, [CsDia])

--definindo os compromissos de um ano
type CsAno = ([CsMes])

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

csMes3 = (3, [ (5, [(10,1), (8,1), (15,2)]), (2, [ (9,2), (14,1)]) ])

--retorna True se o compromisso estiver na lista de compromissos e False caso contrario
buscaC ano mes dia horario = [h | m <- ano, fst(m) == mes, d <- snd(m), fst(d) == dia, h <- snd(d), fst(h) == horario || (fst(h) + snd(h)) > horario]
verificaNaAgenda ano mes dia horario = buscaC ano mes dia horario /= []
ano = [(1,[(2,[(8,2)]),(3,[(8,1), (9,1)])]),(2,[(4,[(10,1)])])]

--buscaC (c:cs) mes dia horario = if fst(c) == mes && 