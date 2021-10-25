--Aluno: Joao Otavio Rodrigues de Castro Manieri
--Data: 17/10/2021
--Trabalho de Programacao

--------------------------------------------------------------------------------Versao Recursao----------------------------------------------------------------------------------------

--tipos pre-definidos
type Livro = String
type Pessoa = String 
type Livros = [Livro]
type Pessoas = [Pessoa]
type Emprestimos = [(Livro, Pessoa)]

--base de dados de livros
baseDeLivros :: Livros
baseDeLivros = ["A peste", "Os Lusiadas","O Castelo","O Leopardo","Lolita","O Processo","Paralelo 42","Hamlet"]

--base de dados de emprestimos
baseDeEmprestimos :: Emprestimos 
baseDeEmprestimos = [("A peste", "Carlos Fabiano"),
 ("A peste", "Luiz Claudio"),
 ("Lolita", "Luiz Claudio"),
 ("Hamlet", "Jose Gustavo"),
 ("Hamlet", "Luiz Claudio"),
 ("O Processo", "Fabiola Nascimento"),
 ("Paralelo 42", "Adriana Castro")]

--searchbooks recebe uma pessoa e a base de empréstimos que estamos trabalhando, aí retorna a lista de livros que essa pessoa pegou emprestado
searchbooks :: Pessoa -> Emprestimos -> Livros
searchbooks pessoa [] = []
searchbooks pessoa (x:xs)
 |snd (x) == pessoa = fst (x) : searchbooks pessoa xs
 |otherwise = searchbooks pessoa xs

--searchdividend recebe um livro e a base de empréstimos que estamos trabalhando, aí retorna a lista de pessoas que tomaram esse livro
searchdividend :: Livro -> Emprestimos -> Pessoas
searchdividend livro [] = []
searchdividend livro (x:xs)
 |fst (x) == livro = snd(x) : searchdividend livro xs
 |otherwise = searchdividend livro xs

--borrowed recebe um livro e retorna uma String, contendo a informação se foi emprestado ou não
borrowed :: Livro -> String
borrowed livro 
 |searchdividend livro baseDeEmprestimos == [] = "Este livro ainda nao foi emprestado!"
 |otherwise = "Este livro ja foi emprestado!"

--searchLoan recebe uma pessoa e retorna um Int com o número de livros que essa pessoa pegou emprestado 
searchLoan :: Pessoa -> Int
searchLoan pessoa = length (searchbooks pessoa baseDeEmprestimos)

--------------------------------------------------------------------------------Versao Compreensao------------------------------------------------------------------------------------

--searchbooks recebe uma pessoa e a base de empréstimos que estamos trabalhando, aí retorna a lista de livros que essa pessoa pegou emprestado
searchbooks_ :: Pessoa -> Livros
searchbooks_ pessoa = [fst x | x <- baseDeEmprestimos, snd x == pessoa]

--searchdividend recebe um livro e a base de empréstimos que estamos trabalhando, aí retorna a lista de pessoas que tomaram esse livro
searchdividend_ :: Livro -> Pessoas
searchdividend_ livro = [snd x | x <- baseDeEmprestimos, fst x == livro]

--borrowed recebe um livro e retorna uma String, contendo a informação se foi emprestado ou não
borrowed_:: Livro -> String
borrowed_ livro 
 |searchdividend_ livro == [] = "Este livro ainda nao foi emprestado!"
 |otherwise = "Este livro ja foi emprestado!"

--searchLoan recebe uma pessoa e retorna um Int com o número de livros que essa pessoa pegou emprestado 
searchLoan_ :: Pessoa -> Int
searchLoan_ pessoa = length (searchbooks_ pessoa)