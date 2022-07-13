type Ponto = (Float, Float)
type Linha = (Ponto, Ponto)

normaVetor :: Linha -> Float
normaVetor linha = sqrt((x1 - x2)**2 + (y1 - y2)**2)
    where 
        x1 = fst(fst linha)
        x2 = fst(snd linha)
        y1 = snd(fst linha)
        y2 = snd(snd linha)

valorY :: Float -> Linha -> Float
valorY x linha = ((y2 - y1)/(x2 - x1))*(x - x1) - y1
    where 
        x1 = fst(fst linha)
        x2 = fst(snd linha)
        y1 = snd(fst linha)
        y2 = snd(snd linha)

main :: IO () 
main = do
putStrLn ("ss")