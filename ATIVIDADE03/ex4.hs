{-Exercício 4:  Utilizando a lista anterior, calcule a soma dos números de Fibonacci pares dos valores que não excedem 4.000.000. (Project Euler 2).-}

geradoraFib :: Integer -> [Integer]
geradoraFib n = map (calcula) [0..n]
                where
                calcula x = round(((1/sqrt(5))*(((1+sqrt(5))/2)^x))-((1/sqrt(5))*(((1-sqrt(5))/2)^x)))         
       
soma :: [Integer] -> Integer
soma xs = sum l
          where
          l = [x | x <- xs, x < 4000000, even x]
main = do 
       print (soma (geradoraFib 1000))
