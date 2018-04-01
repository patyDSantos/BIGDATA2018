{-Exercício 09: Encontre os 10 primeiros anos bissextos.-}

bissexto ano = (ano `rem` 400 == 0) || ((ano `rem` 4 == 0) && (ano `rem` 100 /= 0))

ver_ano :: [Int]
ver_ano = filter bissexto [1..2018]

-- |'main' executa programa principal
main :: IO ()
main = do
    putStrLn $ "Os 10 primeiros anos bissextos são: " ++ show (take 10 ver_ano)