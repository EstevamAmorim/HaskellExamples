import Data.List

type Pessoa = String
type Livro = String
type Palavras = [String]
type Pessoas = [Pessoa]
type Livros = [(Livro, Palavras)]
type Data = (Int, Int, Int)
type BancodeDados = [(Pessoa, Livro, Data)]

maxEmprestimos = 3

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

--Epc10. Operacoes de atualizacao:

jaEmprestado2 :: BancodeDados -> Pessoa -> Livro -> Bool
jaEmprestado2 dBase pessoa livro = if length [1 | (p,l,d) <- dBase, p == pessoa && l == livro] > 0 then True else False

tomaEmprestado2 :: BancodeDados -> Pessoa -> Livro -> Data -> BancodeDados
tomaEmprestado2 dBase pessoa livro dat = if jaEmprestado dBase pessoa livro then error "Ja Emprestado" else (pessoa, livro, dat) : dBase

devolveLivro2 :: BancodeDados -> Pessoa -> Livro -> BancodeDados
devolveLivro2 dBase pessoa livro = if length result == length dBase then error "Nao ha livro emprestado" else result
    where result = [(p,l,d) | (p,l,d) <- dBase, p /= pessoa || l /= livro]

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
print(bd)

print(tomaEmprestado bd "mateus" "Sete Pilares da Sabedoria" (10,12,2022))
--print(tomaEmprestado bd "andre" "Dom Quixote" (10,12,2022)) -- error: "Ja Emprestado"
print(devolveLivro bd "andre" "Dom Quixote")
--print(devolveLivro bd "mateus" "Dom Quixote") -- error: Nao ha livro emprestado

print(tomaEmprestado2 bd "mateus" "Sete Pilares da Sabedoria" (10,12,2022))
--print(tomaEmprestado bd "andre" "Dom Quixote" (10,12,2022)) -- error: "Ja Emprestado"
print(devolveLivro2 bd "andre" "Dom Quixote")
--print(devolveLivro bd "mateus" "Dom Quixote") -- error: Nao ha livro emprestado









