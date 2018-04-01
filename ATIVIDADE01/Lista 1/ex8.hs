{-Exercício 07: Faça uma função que receba um ângulo a e retorne uma tupla contendo o seno da metade desse ângulo utilizando a identidade: sen(x/2) = +- sqrt(1-cos(x)/2)-}

seno :: Double -> (Double, Double)

seno var = (sqrt((1 - cos(var))/2) , -sqrt((1 - cos(var))/2))

-- |'main' executa programa principal
main :: IO ()
main = do
    putStrLn $ "A identidade de (45) é: " ++ show (seno 45)
    putStrLn $ "A identidade de (90) é: " ++ show (seno 90)