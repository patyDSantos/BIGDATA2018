{-Exercício 04: Faça uma função mult35 x que retorne True caso a entrada seja múltiplo de 3 e 5 e False caso contrário.-}
mult35 :: Integer -> Bool
mult35 var = if mod var 3 == 0 && mod var 5 == 0 then True else False

-- |'main' executa programa principal
main :: IO ()
main = do
    putStrLn $ "(300 mult35) - " ++ show (mult35 300)
    putStrLn $ "(15 mult35) - " ++ show (mult35 15)
    putStrLn $ "(50 mult35) - " ++ show (mult35 50)
    putStrLn $ "(24 mult35) - " ++ show (mult35 24)