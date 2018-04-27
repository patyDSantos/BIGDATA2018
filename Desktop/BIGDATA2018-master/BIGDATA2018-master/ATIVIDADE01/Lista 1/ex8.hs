{-Exercício 08: Crie uma lista de anos bissextos desde o ano 1 até o atual.-}

bissexto ano = (ano `rem` 400 == 0) || ((ano `rem` 4 == 0) && (ano `rem` 100 /= 0))

ver_ano :: [Int]
ver_ano = filter bissexto [1..2018]

-- |'main' executa programa principal
main :: IO ()
main = do
    putStrLn $ "Os anos bissextos são: " ++ show ver_ano