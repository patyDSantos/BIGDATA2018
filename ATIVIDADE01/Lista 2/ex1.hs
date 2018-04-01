{-Exercício 1: Crie uma função ehTriangulo que determina se três lados x, y, z podem formar um triângulo.-}

ehTriangulo :: Double -> Double -> Double -> Bool
ehTriangulo x y z
    | abs(x-y) > z || z > (y*x) = False
    | abs(y-z) > x || x > (y*z) = False
    | abs(x-z) > y || y > (x*z) = False    
    | otherwise = True
        
main = do
    putStrLn $ "Verificando: " ++ show (ehTriangulo 7 9 8)
    putStrLn $ "Verificando: " ++ show (ehTriangulo 2 2 10)