-- 1.:

quad :: Int -> Int
quad n = n * n

somaLista :: [Int] -> Int
somaLista [] = 0
somaLista (x:xs) = x + somaLista xs

verfc :: Int -> (Int, Bool)
verfc n = if n >= 0 then (n, True) else (n, False)

listaQuads :: [Int] -> [Int]
listaQuads l = map quad l

somaQuads :: [Int] -> Int
somaQuads l = somaLista (listaQuads l)

verificaSinal :: [Int] -> [(Int, Bool)]
verificaSinal l = map verfc l

-- 2.:

minF :: (Ord t, Num t) => (t -> t) -> t -> t -> t
minF f n menor 
    |n >= 0 = minF (f) (n-1) (if valor < menor then valor else menor)
    |otherwise = menor
        where valor = f n

menorF :: (Ord t, Num t) => (t -> t) -> t -> t
menorF f n = minF f n (f n)

testeIgual :: (Ord t, Num t) => (t -> t) -> t -> Bool 
testeIgual f n 
    |n > 0 = if (f n) == (f (n-1)) then testeIgual f (n-1) else False
    |otherwise = True

testePositivo :: (Ord t, Num t) => (t -> t) -> t -> Bool 
testePositivo f n 
    |n >= 0 = if (f n) >= 0 then testePositivo f (n-1) else False
    |otherwise = True

testeCresc :: (Ord t, Num t) => (t -> t) -> t -> Bool 
testeCresc f n 
    |n > 0 = if (f n) >= (f (n-1)) then testeCresc f (n-1) else False
    |otherwise = True


-- 3.:

trwice :: (Int -> Int) -> Int -> Int
trwice f n = f(f(f(n)))

-- 4.:

iter :: Int -> (t -> t) -> t -> t
iter 0 f x = x
iter n f x = f(iter (n-1) f x)


main :: IO ()
main =  do
print(listaQuads [2,3,4])
print(somaQuads [2,3,4])
print(verificaSinal [2,3,-4])
print(menorF (\x->0-100*x) 4)
print(testeIgual (\x->0-100*x) 4)
print(testeIgual (\x->0) 15)
print(testePositivo (\x->0-100*x) 4)
print(testePositivo (\x->0) 4)
print(testeCresc (\x->100*x) 4)
print(testeCresc (\x->if x `mod` 2 == 0 then 0 else 1) 4)
print(trwice quad 2)
print(iter 4 quad 2)





