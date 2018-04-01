{-Exercício 05: Faça um programa que retorne True caso a entrada seja menor que -1 ou (maior que 1 E múltiplo de 2), e False caso contrário.-}
verify :: Integer -> Bool
verify var = if var < -1 || var > 1 && mod var 2 == 0 then True else False

-- |'main' executa programa principal
main :: IO ()
main = do
    putStrLn $ "(-2 verify) - " ++ show (verify (-2))
    putStrLn $ "(4 verify) - " ++ show (verify 4)
    putStrLn $ "(-3 verify) - " ++ show (verify (-3))
    putStrLn $ "(5 verify) - " ++ show (verify 5)