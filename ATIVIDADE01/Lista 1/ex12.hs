{-Exercício 12: Crie um concatenador de strings que concatena duas strings separadas por espaço.-}

concatenador :: String -> String -> String
concatenador x y = x ++ (' ' : y)

-- |'main' executa programa principal
main :: IO ()
main = do
   putStrLn $ "Concatenador: " ++ show (concatenador "1234" "4321")