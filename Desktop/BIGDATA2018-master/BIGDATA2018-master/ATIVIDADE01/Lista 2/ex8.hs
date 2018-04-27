{-Exercício 8:   Faça uma função que calcule o elemento (i,j) do triângulo de pascal.-}

trianguloP :: Integer -> Integer -> Integer
trianguloP i j = verificar i j
    where
      fatorial 1 aux = aux
      fatorial var aux = fatorial (var-1)(aux*var)
      verificar i j = (fatorial (i+j) 1) `div` (fatorial j 1)
main :: IO ()
main = do
    putStrLn $ "O triângulo de Pascal é : " ++ show (trianguloP 5 3)
    putStrLn $ "O triângulo de Pascal é : " ++ show (trianguloP 4 1)
    putStrLn $ "O triângulo de Pascal é : " ++ show (trianguloP 2 2)