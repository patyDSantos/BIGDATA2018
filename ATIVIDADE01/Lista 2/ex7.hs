{-Exercício 7:  Faça uma função que calcule o coeficiente binomial de (m,n).-}

binomio :: Integer -> Integer -> Integer
binomio n 0 = 1
binomio n m = if m == n then 1
              else binomio (n-1)m + binomio (n-1)(m-1)
main :: IO ()
main = do
    putStrLn $ "O coeficiente binomial é : " ++ show (binomio 5 3)
    putStrLn $ "O coeficiente binomial é : " ++ show (binomio 4 1)
    putStrLn $ "O coeficiente binomial é : " ++ show (binomio 3 0)
