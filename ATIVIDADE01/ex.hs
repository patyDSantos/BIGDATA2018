{-
-- Ex. 2
-- Exercício 02: Faça uma função mult3 x que retorne True caso a entrada seja múltiplo de 3 e False caso contrário.
-- Data: 03/03/2018
-}
mult3 :: Integer -> Bool
mult3 var = if mod var 3 == 0 then True else False

-- |'main' executa programa principal
main :: IO ()
main = do
    print ("Ex. 2")
