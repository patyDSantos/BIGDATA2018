{-Exercício 01: Execute as seguintes operações utilizando o menor número de parênteses: 2.3 + 5, 2 + 2.3 + 1, 3^4 + 5.2^5 + 1-}
teste01 :: (Double, Double, Double)
teste01 = (2*3+5 , 2+2*3+1 , 3**4+5*2**5+1)

-- |'main' executa programa principal
main :: IO ()
main = do
    putStrLn $ "Resultado:" ++ show teste01