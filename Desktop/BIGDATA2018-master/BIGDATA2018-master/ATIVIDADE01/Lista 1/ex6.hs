{-Exercício 06: Faça uma função que recebe um tipo Integer e retorna ele dividido por 2.-}

div2d :: Integer -> Double

div2d var = (fromIntegral var) / 2

-- |'main' executa programa principal
main :: IO ()
main = do
    putStrLn $ "A divisão de (10) por 2 é: " ++ show (div2d 10)
    putStrLn $ "A divisão de (5) por 2 é: " ++ show (div2d 5)
  