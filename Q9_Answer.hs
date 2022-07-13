import Data.List

type Pessoa = String
type Livro = String
type Palavras = [String]
type Pessoas = [Pessoa]
type Livros = [(Livro, Palavras)]
type Data = (Int, Int, Int)
type BancodeDados = [(Pessoa, Livro, Data)]

maxEmprestimos = 3

-- 1. Operacoes de consulta:

livrosPorPessoa :: BancodeDados -> Pessoa -> [Livro]
livrosPorPessoa [] _ = []
livrosPorPessoa ((p, l, d) : resto) pessoa
    |p == pessoa = l : livrosPorPessoa resto pessoa
    |otherwise = livrosPorPessoa resto pessoa

pessoasPorLivro :: BancodeDados -> Livro -> [Pessoa]
pessoasPorLivro [] _ = []
pessoasPorLivro ((p, l, d) : resto) livro
    |l == livro = p : pessoasPorLivro resto livro
    |otherwise = pessoasPorLivro resto livro

estaEmprestado :: BancodeDados -> Livro -> Bool
estaEmprestado [] _ = False
estaEmprestado ((p, l, d) : resto) livro
    |l == livro = True 
    |otherwise = estaEmprestado resto livro
    
quantLivrosPorPessoa :: BancodeDados -> Pessoa -> Int
quantLivrosPorPessoa [] _ = 0
quantLivrosPorPessoa ((p, l, d) : resto) pessoa
    |p == pessoa = 1 + quantLivrosPorPessoa resto pessoa
    |otherwise = quantLivrosPorPessoa resto pessoa


-- 2. Operacoes de atualizacao:

jaEmprestado :: BancodeDados -> Pessoa -> Livro -> Bool
jaEmprestado [] _ _ = False
jaEmprestado ((p, l, d) : resto) pessoa livro
    |p == pessoa && l == livro = True
    |otherwise = jaEmprestado resto pessoa livro

tomaEmprestado :: BancodeDados -> Pessoa -> Livro -> Data -> BancodeDados
tomaEmprestado dBase pessoa livro dat = if jaEmprestado dBase pessoa livro then error "Ja Emprestado" else (pessoa, livro, dat) : dBase

devolveLivro :: BancodeDados -> Pessoa -> Livro -> BancodeDados
devolveLivro ((p, l, d): resto) pessoa livro
    |p == pessoa && l == livro = resto
    |otherwise = (p, l, d) : devolveLivro resto pessoa livro
devolveLivro [] pessoa livro = error "Nao ha livro emprestado"


-- ModificacoesDB

tomaEmprestadoLimite :: BancodeDados -> Pessoa -> Livro -> Data -> BancodeDados
tomaEmprestadoLimite dBase pessoa livro dat
    |quantLivrosPorPessoa dBase pessoa < maxEmprestimos = tomaEmprestado dBase pessoa livro dat
    |otherwise = error "Excesso de emprestimos"

buscaPorPalavras :: Livros -> Palavras -> [Livro]
buscaPorPalavras [] _ = []
buscaPorPalavras ((livro, palavras): resto) palavrasBusca 
    |intsc > 0 = livro : buscaPorPalavras resto palavrasBusca
    |otherwise = buscaPorPalavras resto palavrasBusca
        where
            intsc = length (intersect palavras palavrasBusca)

detectarVencidos :: BancodeDados -> Data -> [(Pessoa, Livro, Data)]
detectarVencidos [] _ = []
detectarVencidos ((pessoa, livro, (dv, mv, av)): resto) (d, m, a)
    |av < a || (av == a && (mv < m || (mv == m && dv < d))) = (pessoa, livro, (dv, mv, av)) : detectarVencidos resto (d, m, a)
    |otherwise = detectarVencidos resto (d, m, a) 


pessoas = ["andre", "joao", "mateus", "luis"]

livros = [ ("A Mente Nova do Rei", ["drama","fantasia"])
         , ("Cem Anos de Solidao", ["historico", "drama"])
         , ("Dom Quixote", ["drama","fantasia"])
         , ("Sete Pilares da Sabedoria", ["historico", "aventura"])
         , ("O Senhor dos Aneis", ["fantasia", "aventura"]) ]

bd = [ ("andre", "A Mente Nova do Rei", (4,12,1999))
     , ("andre", "Dom Quixote",  (4,12,1999)) 
     , ("andre", "Sete Pilares da Sabedoria", (5,5,2022)) 
     , ("joao", "Cem Anos de Solidao", (27,5,2022))
     , ("joao", "Sete Pilares da Sabedoria", (3,5,2022)) 
     , ("mateus", "A Mente Nova do Rei", (3,4,2022))
     , ("luis", "Sete Pilares da Sabedoria", (4,4,2022))
     , ("luis", "O Senhor dos Aneis", (27,5,2022))
     , ("luis", "Dom Quixote", (5,5,2022)) ]

main :: IO ()
main =  do
print(livrosPorPessoa bd "andre")
print(livrosPorPessoa bd "joao")
print(livrosPorPessoa bd "luis")
print(pessoasPorLivro bd "Sete Pilares da Sabedoria")
print(pessoasPorLivro bd "Cem Anos de Solidao")
print(pessoasPorLivro bd "O Senhor dos Aneis")
print(pessoasPorLivro bd "O")
print(estaEmprestado bd "Sete Pilares da Sabedoria")
print(estaEmprestado bd "Dom Quixote")
print(estaEmprestado bd "A Mente Nova do Rei")
print(quantLivrosPorPessoa bd "andre")
print(quantLivrosPorPessoa bd "mateus")
print(bd)
print(tomaEmprestado bd "mateus" "Sete Pilares da Sabedoria" (10,12,2022))
--print(tomaEmprestado bd "andre" "Dom Quixote" (10,12,2022)) -- error: "Ja Emprestado"
print(devolveLivro bd "andre" "Dom Quixote")
--print(devolveLivro bd "mateus" "Dom Quixote") error: Nao ha livro emprestado
print(tomaEmprestadoLimite bd "mateus" "O Senhor dos Aneis" (22,4,2022))
--print(tomaEmprestadoLimite bd "andre" "Cem Anos de Solidao" (22,4,2022)) -- error: Excesso de emprestimos
print(buscaPorPalavras livros ["drama","fantasia"])
print(buscaPorPalavras livros ["cronica"])
print(detectarVencidos bd (5,12,1999))
print(detectarVencidos bd (3,5,2022))
print(detectarVencidos bd (3,5,2021))
print(detectarVencidos bd (3,5,2023))
print(detectarVencidos bd (6,6,2022))




{-buscaPorPalavras :: Livros -> Palavras -> Int -> Livros -> Livros
buscaPorPalavras ((livro, palavras): resto) palavrasBusca maisPalavrasComum livrosEncontrados
    |intsc == maisPalavrasComum && intsc > 0 = buscaPorPalavras resto palavrasBusca intsc livro : livrosEncontrado
    |intsc > maisPalavrasComum = buscaPorPalavras resto palavrasBusca intsc [livro] 
    |otherwise = buscaPorPalavras resto palavrasBusca maisPalavrasComum livrosEncontrados
        where
            intsc = length intersect palavras palavrasBusca 
buscaPorPalavras [] _ _ livrosEncontrados = livrosEncontrados-}






