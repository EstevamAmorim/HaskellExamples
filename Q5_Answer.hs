venda :: Int -> Int
venda 0 = 7
venda 1 = 2
venda 2 = 5

vendaMaxima :: Int -> Int
vendaMaxima n 
    | n == 0 || venda n > venda maior = n 
    | otherwise = maior
        where 
            maior = vendaMaxima(n - 1)

semVendas :: Int -> Int
semVendas n
    | venda n == 0 = n
    | n == 0 || sem == n = n + 1
    | otherwise = sem
        where sem = semVendas( n - 1)
            
{- A função retorna a semana sem vendas mais recente -}

quantasSemVendas :: Int -> Int
quantasSemVendas n 
    | venda n == 0 = 1 + q
    | otherwise = q
        where q = if n == 0 then 0 else quantasSemVendas(n-1)
        
quantasXVendas :: Int -> Int -> Int
quantasXVendas n x
    | venda n == x = 1 + q
    | otherwise = q
        where q = if n == 0 then 0 else quantasXVendas (n-1) x

quantasSemVendas2 :: Int -> Int
quantasSemVendas2 n = quantasXVendas n 0

nAnd :: Bool -> Bool -> Bool
nAnd x y = not x && y

max3 :: Int -> Int -> Int -> Int
max3 n m p = max n (max m p)

numEquallMax :: Int -> Int -> Int -> Int
numEquallMax n m p
    | n == m && m == p = 2
    | ((n == mx) && (n == m || n == p)) || (m == mx) && (m == p)  = 1
    | otherwise = 0
        where mx = max3 n m p

main :: IO () 
main = do
putStrLn (show (vendaMaxima 2))