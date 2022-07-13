todosQuatroIguais :: Int -> Int -> Int -> Int -> Bool
todosQuatroIguais v1 v2 v3 v4 = v1 == v2 && v2 == v3 && v3 == v4 

todosIguais :: Int -> Int -> Int -> Bool
todosIguais v1 v2 v3 = v1 == v2 && v2 == v3

todosQuatroIguais2 :: Int -> Int -> Int -> Int -> Bool
todosQuatroIguais2 v1 v2 v3 v4 = todosIguais v1 v2 v3 && v3 == v4

{- Q3 - O caso em que o primeiro e o último parametros da função são iguais não 
        é verificado, portanto a função não pode assegurar que todos os seus parametros são iguais -}

teste :: Int -> Int -> Int -> Bool
teste n m p = ((n + m + p) == 3 * p)

{-  Q4 -Não se comporta da mesma maneira, casos como 1 1 1 ou 1 2 3 são interpretados corretamente, 
        porém casos como 2 4 3 apresentam falsos afirmativos -}

quantosIguais :: Int -> Int -> Int -> Int
quantosIguais v1 v2 v3 
    | v1 == v2 && v2 == v3 = 3
    | v1 == v2 || v1 == v3 || v2 == v3 = 2
    | otherwise = 0

main :: IO () 
main = do
putStrLn (show (teste 2 4 3))