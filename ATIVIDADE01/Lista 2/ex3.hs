{-Exercício 3:  Implemente uma função que faz a multiplicação etíope entre dois números.-}

etiope :: Int -> Int -> Int
etiope x y = divide x y 0
      where
        soma x y      = if (x `mod` 2 == 0) then 0 else y
        divide 1 y z = z + y
        divide x y z = divide (x `div` 2) (2*y) ((soma x y) + z)
        
main :: IO ()
main = do
    putStrLn $ "A multiplicacao etiope : " ++ show (etiope 72 535)
    putStrLn $ "A multiplicacao etiope : " ++ show (etiope 17 34)