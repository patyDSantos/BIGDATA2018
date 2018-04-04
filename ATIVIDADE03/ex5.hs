{-Exercício 5:  Faça uma função para calcular o produto escalar entre dois vetores.-}

produtoEsc :: [Int] -> [Int] -> Int
produtoEsc as bs = sum (zipWith (*) as bs)
       
main = do 
       print(produtoEsc [2,1] [4,3])
