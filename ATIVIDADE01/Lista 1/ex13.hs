{-Exercício 13: Dada a string “0123456789”, crie uma lista com os dígitos em formato Integer.-}

transformador :: String -> [Int]
transformador string = [read [x] :: Int | x <- string]

-- |'main' executa programa principal
main :: IO ()
main = do
   putStrLn $ "Transformador: " ++ show (transformador "0123456789")