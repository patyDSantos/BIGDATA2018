{-Exercício 1:  Crie uma função divisivel20 x que retorna verdadeiro se x for divisível por todos os números de 1 a 20.-}

divisivel20 :: Int -> Bool
divisivel20 x = if length (filter (== True) var) == 20
                then True
                else False
                where l = map (x `mod`) [1..20] 
                      var =  map (== 0) l
main = do 
       print(divisivel20 400)
       print(divisivel20 4400)