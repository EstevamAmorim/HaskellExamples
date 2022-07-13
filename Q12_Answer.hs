-- 1.:
f :: [Int] -> [t] -> [t]
f x l_t = l_t ++ [l_t !! (a-1) | a <- reverse x]

-- 2.:
result :: Int -> [(Int, Int, Int)]
result n = [(x, y, z) | x <- [0..n], y <- [x..n], z <- [x..n], x^2 + y^2 == z^2] 

-- 3.:
comp :: Int -> Int  -> Int
comp nj n = if nj == n || nj == -1 then n - abs(nj) else nj

diag :: (Num num) => [[num]] -> Int -> Int -> Int -> Int -> Int -> num
diag _ _ _ _ 0 _ = 1
diag l i j direc a n = (l !! i !! j) * diag l (i-1) (comp (j+direc) n) direc (a-1) n

summ :: (Num num) => [[num]] -> Int -> Int -> Int -> Int -> Int -> num
summ _ _ _ _ 0 _ = 0
summ l i j direc a n = diag l i j direc n n + summ l i (comp (j+1) n) direc (a-1) n

det :: (Num num) => [[num]] -> Int -> num
det l n = summ l (n-1) (n-1) (-1) n n - summ l (n-1) 1 1 n n

-- 4.:
f1 :: [t]->[[t]] 
f1 l = [[l!!(a-1) | a <- [1..length l], a `mod` 2 == 1], [l!!(b-1) | b <- [2..length l], b `mod` 2 == 0]]

-- 5.:
sublistas :: [t] -> Int -> [[t]] 
sublistas [] _ = [[]]
sublistas (x:xs) n = [x:sublist | sublist <- sublistas xs (n - 1)] ++ if length xs >= n then sublistas xs n else []

--exemplo - a: [[bcd], [bc], [bd], [b], [cd], [c], [d], []]  ~ [[bcd]]--

-- 7.:
m :: [[Int]]
m = [[2,3,4], [5,6,7], [8,9,10]]

m1 :: Int -> Int -> Int -> [[Int]]
m1 ini lin col = [[(l*col+ini)..(l*col+(col-1)+ini)]|l <- [0..(lin-1)]]

t :: [[Int]]->[[Int]]
t [[]] = [[]]
t m = [[m!!i!!j | i <- [0.. length m-1]] | j <- [0..length (head m)-1]]

main :: IO ()
main =  do
print(f [2,1,4] ['a', 'b', 'c', 'd']) 
print(show(result 20))
print(show(det [[1,9,5], [3,7,8], [10,4,2]] 3))
print(show(f1 ['a', 'b', 'c', 'd', 'e']))
print(show(sublistas ['a', 'b', 'c', 'd'] 3 ))
print(show(m1 2 2 3))
print(show(t (m1 2 2 3)))




