--programador: Joao Otavio Rodrigues de Castro Manieri

--base de dados dos livros
basedelivros :: [String]
basedelivros = ["Diário de Um Banana","Jogos Vorazes","Clean Code","Haskell Aprendendo e Se Divertindo","As aventuras do marquinhos","Marcelo e as crianças","Cleitinho e marcelinho nas aventuras","O trem"]

--livros emprestados
basedeemprestimos :: [(String, String)] 
basedeemprestimos = [("Diário de Um Banana", "Marcelinho"),("Jogos Vorazes", "Pedro"),("Clean Code", "José"),("Haskell Aprendendo e Se Divertindo", "Leticia"),("As aventuras do marquinhos", "Rafael"),("Marcelo e as crianças", "Luquinhas"),("Cleitinho e marcelinho nas aventuras", "Pedro Josué")]

--a funcao recebe uma determinada pessoa e os empréstimos e a funcao retornara os livros que estao emprestados a essa pessoa

procuralivros :: String -> [String]
procuralivros pessoa = map fst (filter ((== pessoa).snd) basedeemprestimos)

--a funcao recebera um determinado livro e os emprestimos e a funcao retornara os tomadores dos livro

searchBorrowers :: String -> [String]
searchBorrowers book = map snd (filter ((== book).fst) basedeemprestimos)


emprestadoo :: Eq x=> x -> [(x,x)] -> Bool
emprestadoo n [] = False
emprestadoo n (x:xs)
 |n == snd x = True
 |otherwise = emprestadoo n xs

--a funcao recebe um determinado livro e a funcao retornara se o livro foi emprestado ou nao
jaemprestado :: String -> String
jaemprestado x 
 |emprestadoo x basedeemprestimos = "Este book ainda nao foi emprestado!"
 |otherwise = "Este book ja foi emprestado!"

--a funcao recebe uma pessoa e retornara a quantidade de livros que essa pessoa emprestou 
procuradividendos :: String -> Int
procuradividendos pessoa = length (procuralivros pessoa)