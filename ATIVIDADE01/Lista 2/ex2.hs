{-Exercício 2: Crie uma função tipoTriangulo que determina o tipo do triângulo formado pelos três lados x, y, z.-}

tipoTriangulo :: Double -> Double -> Double -> String
tipoTriangulo x y z
    | (x==y) && (y==z) && (x==z) = "Triangulo Equilatero"
    | (x/=y) && (y/=z) && (x/=z) = "Triangulo Escaleno"
    | (x==y) || (x==z) || (y==z) = "Triangulo Isosceles"
        
main :: IO ()
main = do
    putStrLn $ "Verificando: " ++ show (tipoTriangulo 7 9 8)
    putStrLn $ "Verificando: " ++ show (tipoTriangulo 2 2 2)
    putStrLn $ "Verificando: " ++ show (tipoTriangulo 10 20 20)