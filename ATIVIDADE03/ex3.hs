{-Exercício 3:  Crie a lista de números de Fibonacci utilizando uma função geradora.-}

geradoraFib :: Integer -> [Integer]
geradoraFib n = map (calcula) [0..n]
                where
                calcula x = round(((1/sqrt(5))*(((1+sqrt(5))/2)^x))-((1/sqrt(5))*(((1-sqrt(5))/2)^x)))
				
main = do 
       print(geradoraFib 10)