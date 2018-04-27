{-Exercício 02: Faça uma função mult3 x que retorne True caso a entrada seja múltiplo de 3 e False caso contrário.-}
mult3 :: Integer -> Bool
mult3 var = if mod var 3 == 0 then True else False

-- |'main' executa programa principal
main :: IO ()
main = do
    putStrLn $ "(300 mult3) - " ++ show (mult3 300)
    putStrLn $ "(15 mult3) - " ++ show (mult3 15)
    putStrLn $ "(8 mult3) - " ++ show (mult3 8)
    putStrLn $ "(109 mult3) - " ++ show (mult3 109)