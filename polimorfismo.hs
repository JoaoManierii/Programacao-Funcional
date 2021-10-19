--Data: 18/10/2021
--Nome: Joao Otavio Manieri

achaItem :: (Integral z) => z -> (String,z)
achaItem 1234 = ("Oleo DoBom, 1l", 195)
achaItem 4756 = ("Chocolate Cazzeiro, 250g" ,180)
achaItem 3216 = ("Arroz DoBom, 5Kg", 213)
achaItem 5823 = ("Arroz DoBom, 5Kg", 213)
achaItem 4719 = ("Queijo Mineirim, 1Kg" , 449)
achaItem 6832 = ("Iogurte Maravilha, 1Kg" , 499)
achaItem 1112 = ("Rapadura QuebraDente, 1Kg", 80)
achaItem 1111 = ("Sal Donorte, 1Kg", 221)
achaItem 1113 = ("Cafe DoBom, 1Kg", 285)
achaItem 1115 = ("Biscoito Bibi, 1Kg",80)
achaItem 3814 = ("Sorvete QGelo, 1l", 695)

formataCentavos ::(Integral z,Show z) => z -> String
formataCentavos z
 |mod z 10 <10 && z<10 = show ((div) z 100)  ++ "." ++ "0" ++ show ((mod) z 100)
 |otherwise = show ((div) z 100)  ++ "." ++ show ((mod) z 100)


formataLinha :: ((Integral z),Show z)=>(String,z) -> String
formataLinha (z,y) = (z)++ ((replicate)(30 -(length z)-(length(show y))) '.')++(formataCentavos y) ++ "\n"

geraRecibo :: (Integral z,Show z) => [z] -> String
geraRecibo lc = formataRecibo(fazRecibo lc)

formataLinhas :: (Integral z,Show z) => [(String,z)] -> String
formataLinhas [] = []
formataLinhas (z:xs) = formataLinha z ++ formataLinhas xs

fazRecibo :: (Integral z,Show z) => [z]-> [(String, z)]
fazRecibo [] = [] 
fazRecibo (z:xs) = achaItem z:fazRecibo xs

geraTotal :: Num b => [(a,b)] -> b 
geraTotal [] = 0
geraTotal (z:xs) = snd z + geraTotal (xs)


formataRecibo :: (Integral z,Show z) => [(String,z)] -> String 
formataRecibo [] = []
formataRecibo z = formataLinhas z ++ formataTotal (geraTotal (z)) 


formataTotal :: (Integral z,Show z) => z -> String
formataTotal z = "Total" ++ ((replicate)(30 - (length(formataCentavos z)+6)) '.') ++ "$" ++ (formataCentavos z) 
