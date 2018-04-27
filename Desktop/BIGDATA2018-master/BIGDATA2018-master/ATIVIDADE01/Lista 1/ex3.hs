{-Exercício 03: Faça uma função mult5 x que retorne True caso a entrada seja múltiplo de 5 e False caso contrário.-}
mult5 :: Integer -> Bool
mult5 var = if mod var 5 == 0 then True else False

-- |'main' executa programa principal
main :: IO ()
main = do
    putStrLn $ "(300 mult5) - " ++ show (mult5 300)
    putStrLn $ "(15 mult5) - " ++ show (mult5 15)
    putStrLn $ "(8 mult5) - " ++ show (mult5 8)
    putStrLn $ "(109 mult5) - " ++ show (mult5 109)