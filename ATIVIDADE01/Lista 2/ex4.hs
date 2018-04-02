{-Exercício 4:  Faça uma função que determine se um número é primo.-}

primo :: Integer -> Bool

primo x = primo x (x-1)
  where
    primo a 1 = True
    primo a b = if (a `mod` b == 0) then False 
                else primo a (b-1)

main :: IO ()
main = do
    putStrLn $ "Verifica se eh primo : " ++ show (primo 2)
    putStrLn $ "Verifica se eh primo : " ++ show (primo 15)