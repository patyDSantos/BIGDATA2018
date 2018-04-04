{-Exercício 3:  Crie a lista de números de Fibonacci utilizando uma função geradora.-}

listaFib :: Integer -> [Integer]
listaFib n = map (gera) [0..n]
                where
                gera x = round(((1/sqrt(5))*(((1+sqrt(5))/2)^x))-((1/sqrt(5))*(((1-sqrt(5))/2)^x)))
main = do 
       print(listaFib 30)