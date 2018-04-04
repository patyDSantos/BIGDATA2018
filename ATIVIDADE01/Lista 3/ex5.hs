{-Exercício 5:  Faça uma função para calcular o produto escalar entre dois vetores.-}

produtoEscalar :: [Int] -> [Int] -> Int
produtoEscalar as var = sum (zipWith (*) as var)
       
main = do 
       print(produtoEscalar [4,8] [4,12])
