{-Exercício 10: Encontre os 10 últimos anos bissextos (dica: use a função length para determinar o tamanho da lista).-}

bissexto ano = (ano `rem` 400 == 0) || ((ano `rem` 4 == 0) && (ano `rem` 100 /= 0))

ver_ano :: [Int]
ver_ano = filter bissexto [1..2018]

-- |'main' executa programa principal
main :: IO ()
main = do
    putStrLn $ "A 10 últimos anos bissextos são: " ++ show (drop xxx ver_ano)
      where xxx = (length ver_ano)-10