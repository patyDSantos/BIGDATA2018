{-Exercício 5:  Faça uma função que calcule a soma dos dígitos de um número.-}

somarDigitos :: Integer -> Integer
somarDigitos var = soma var 0
    where
      divisao var = var `mod` (10)
      soma   0 resto  = resto
      soma   var resto  = soma (var `div` 10) (divisao var + resto)
main :: IO ()
main = do
    putStrLn $ "A soma é : " ++ show (somarDigitos 22)
    putStrLn $ "A soma é : " ++ show (somarDigitos 45)
    putStrLn $ "A soma é : " ++ show (somarDigitos 222)