{-Exercício 11: Crie uma tupla em que o primeiro elemento tem metade dos anos bissextos e o segundo elemento a outra metade.-}

bissexto ano = (ano `rem` 400 == 0) || ((ano `rem` 4 == 0) && (ano `rem` 100 /= 0))

ver_ano :: [Int]
ver_ano = filter bissexto [1..2018]

-- |'main' executa programa principal
main :: IO ()
main = do
    putStrLn $ "A primeira dos anos bissextos é: " ++ show (take metade ver_ano)++ "\n E a segunda metade é: " ++ show (drop metade ver_ano)
      where metade = div(length ver_ano)2