{-Exercício 2:   Faça uma função que calcule a soma da diagonal principal de uma matriz.-}

somaDiagonal :: [[Int]] -> Maybe Int
somaDiagonal matriz
  |linhas /= colunas = Nothing
  |otherwise = Just (sum $ zipWith (!!) matriz [0..])
    where
      linhas = length(matriz)
      colunas = length(matriz !! 0)
main :: IO ()
main = do

    let matriz1 = [[ 11,22, 33, 44],[ 12, 23, 34, 45],[ 13,24,35,46]]
    putStrLn $ "A soma da diaginal principal da matriz é : " ++ show (somaDiagonal matriz1)

    let matriz2 = [[ 11,22, 33, 44],[ 12, 23, 34, 45],[ 13,24,35,46], [14,25,36,47]]
    putStrLn $ "A soma da diaginal principal da matriz é : " ++ show (somaDiagonal matriz2)
