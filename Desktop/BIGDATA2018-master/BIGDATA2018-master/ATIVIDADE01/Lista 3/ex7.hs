{-Exercício 7:   Implemente uma função collatzLen x que retorna o tamanho da lista formada pela aplicação repetida de collatz sobre o valor x até que essa chegue no número 1.-}

collatz :: Int -> Int
collatz x 
        |even x = x `div` 2
        |odd x = (3 * x)+ 1


collatzLen :: Int -> Int
collatzLen x = length (collatzTotal x)
          where 
          collatzTotal x
           |x == 1 = x:[]
           |otherwise = x:collatzTotal (collatz x)
           
main = do 
       print(collatzLen 500)
       print(collatzLen 11)
