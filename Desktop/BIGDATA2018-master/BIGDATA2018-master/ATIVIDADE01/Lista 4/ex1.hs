{-Exercício 1:   Faça uma função que gere uma matriz identidade de tamanho n.-}

matrizIdentidade:: Integer -> [[Integer]]
matrizIdentidade n = [[ matrizIdentidade' i j | i <- [1..n]] | j <- [1..n]]
  where
    matrizIdentidade' x y
      | x==y = 1
      |otherwise = 0
main :: IO ()
main = do
    putStrLn $ "Com n = 3 a identidade é : " ++ show (matrizIdentidade 3)
    putStrLn $ "Com n = 10 a identidade é : " ++ show (matrizIdentidade 10)
