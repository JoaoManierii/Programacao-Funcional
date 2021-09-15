--Programdor: Joao Otavio Rodrigues de Castro Manieri
--BSI Noturno 
--Data:29/08/2021
--Estudo Dirigido

{-
A function in Haskell that, given a certain 21st century date described in the format (day,month,year), gets the corresponding day of the week (without using Zeller congruence)
-}

type Dia = Int
type Mes = Int
type Ano = Int
type Data = (Dia,Mes,Ano)


-- A funcao bissexto devolve se o ano eh bissexto ou nao 
bissexto :: Ano -> Bool
bissexto x 
 |mod x 400 == 0                 = True
 |mod x 4 == 0 && mod x 100 /= 0 = True
 |otherwise                      = False


-- A funcao numDeDiasEmCadaMesDeUmAno devolve quantos dias tem cada mes de determinado ano
numDeDiasEmCadaMesDeUmAno :: Ano -> [Int]
numDeDiasEmCadaMesDeUmAno ano = [31,fev,31,30,31,30,31,31,30,31,30,31]
 where fev
        |bissexto ano = 29
        |otherwise = 28 

--A funcao numDeDias fornecida uma data, fornece o número de dias desde 31 de Dezembro de 2000
numDeDias :: Data -> Int
numDeDias (dia,mes,ano) = dia 
     + sum (take (mes-1) (numDeDiasEmCadaMesDeUmAno ano))
     + (ano-2001)*365 + (ano-2001)`div`4

-- A funcao dado um número na faixa de 0 a 6 retorna o nome do dia
nomeDoDia :: Int -> String
nomeDoDia y
 |(y==0) = "Domingo"
 |(y==1) = "Segunda" 
 |(y==2) = "Terca" 
 |(y==3) = "Quarta" 
 |(y==4) = "Quinta" 
 |(y==5) = "Sexta"
 |(y==6) = "Sabado" 


--A funcao menor subtrai 7 de um numero ate que ele se torne  menor que o numero 7. 
menor :: Int -> Int
menor x
 |x < 7 = x
 |otherwise = menor (x-7)

-- A a funcao diaDaSemana2 determinada data do século XXI descrita no formato (dia,mês,ano), obtém o dia da semana correspondente
diaDaSemana :: Data -> String 
diaDaSemana (dia, mes, ano) 
 |ano > 2100 = "Voce esta fora do seculo XXI digite valores menores que (31,12,2100)"
 |otherwise = nomeDoDia (menor (numDeDias(dia, mes, ano)))
