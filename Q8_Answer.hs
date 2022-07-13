import Prelude hiding (product, and, concat, unzip)

product :: [Int] -> Int
product [] = 1
product (a:xs) = a * (product xs)

and :: [Bool] -> Bool
and [] = True
and (a:xs) = a && (and xs)

concat :: [[Int]] -> [Int] 
concat [] = []
concat (a:xs) = a ++ (concat xs)

somaTriplas :: (Num n) => [(n,n,n)] -> n
somaTriplas [] = 0
somaTriplas ((c, d, e):xs) = (c + d + e) + (somaTriplas xs)

unzip :: [(Int, Int)] -> ([Int], [Int])
unzip l = (unZipLeft l, unZipRight l)

unZipLeft :: [(Int, Int)] -> [Int] 
unZipLeft [] = []
unZipLeft ((a, b):xs) = a : unZipLeft xs

unZipRight :: [(Int, Int)] -> [Int] 
unZipRight [] = []
unZipRight ((a, b):xs) = b : unZipRight xs


main :: IO ()
main =  do
print(product [2,6,10])
print(and [True, False])
print(concat [[3,2],[3]])
print(somaTriplas [(3,2,1),(2,2,2)])
print(unzip [(2,4), (3,5), (4,78)])
