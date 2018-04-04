{-Exercício 8:  Encontre o número x entre 1 e 1.000.000 que tem a maior sequência de Collatz. (Project Euler 14).-}

import Data.List
import Data.Maybe

collatz :: Int -> Int
collatz x 
        |even x = x `div` 2
        |odd x = (3 * x)+1

collatzLen :: Int -> Int
collatzLen x = length (collatzTotal x)
          where 
          collatzTotal x
           |x == 1 = x:[]
           |otherwise = x:collatzTotal (collatz x)

{-verSeq = (fromMaybe 0 (elemIndex (maximum xs) xs)) + 1
       where xs = map collatzLen [1..1000000]-}
verSeq = (fromMaybe 0 (elemIndex (maximum xs) xs)) + 1
       where xs = map collatzLen [1..1000]
main = do
       print (verSeq)