{-Exercício 6:  Faça uma função que calcule a persistência aditiva de um número.-}

{- Efetuar a somatória dos dígitos de um número até ele se tornar um único dígito.-}

persistencia :: Integer -> Integer
persistencia var = somatoria var 0 - 1
    where
    divisao var = var `mod` (10)
    soma 0 resto 1 = 0
    soma 0 resto k = resto
    soma var resto k = soma (var `div` 10) (divisao var + resto) (k + 1)
    somatoria 0 resto = resto
    somatoria var resto = somatoria (soma var 0 0) (resto + 1)
    
main :: IO ()
main = do
    putStrLn $ "A persistencia aditiva é : " ++ show (persistencia 12345)
    putStrLn $ "A persistencia aditiva é : " ++ show (persistencia 45)
    putStrLn $ "A persistencia aditiva é : " ++ show (persistencia 123)
