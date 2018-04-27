{-Exercício 3:   Faça uma função que calcule a soma da diagonal secundária de uma matriz.-}

somaDiagonalSec var = sum $ map head $ zipWith drop [0..] (map reverse matriz)

matriz = [[ 11,22, 33, 44],[ 12, 23, 34, 45],[ 13,24,35,46], [14,25,36,47]]


main :: IO ()
main = do

  putStrLn $ "A soma da diaginal secundária da matriz é : " ++ show (somaDiagonalSec matriz)