uppercase :: Char -> Char
uppercase c = if c >= 'a' && c <= 'z' then toEnum (fromEnum c - 32) else c

charParaInt :: Char -> Int
charParaInt c = if c >= '0' && c <= '9' then fromEnum c - 48 else 0

imprimeDigito :: Char -> String
imprimeDigito c = case c of 
    {'0'->"zero"; '1'->"um";'2'->"dois";'3'->"três";'4'->"quatro";'5' ->"cinco";'6'->"seis";'7'->"sete";'8'->"oito";'9' ->"nove";_->"invalid input"}

romanoParaString :: Char -> String
romanoParaString c = case c of
    {'I'->"um"; 'V'->"cinco";'X'->"dez";'L'->"cinquenta";'C'->"cem";'D' ->"quinhentos";'M'->"mil";_->"invalid input"}

emTresLinhas :: String -> String -> String -> String
emTresLinhas s1 s2 s3 = s1 ++ "\n" ++ s2 ++ "\n" ++ s3

replica :: String -> Int -> String
replica s i
    |i == 0 = ""
    |otherwise = s ++ replica s (i - 1)


main :: IO () 
main = do