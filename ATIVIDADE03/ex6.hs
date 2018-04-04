{-Exercício 6:  Crie a função collatz x que retorna x/2, se x for par e (3x+1) se for ímpar.-}

collatz :: Int -> Int
collatz x 
        |even x = x `div` 2
        |odd x = (3 * x)+1
		
main = do 
       print(collatz 10)