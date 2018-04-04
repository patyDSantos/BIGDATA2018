{-Exercício 2:  Crie uma função projectEuler5 que retorna o primeiro número natural que retorna True para a função do exercício anterior. Pense em como reduzir o custo computacional.-}
divisivel20 :: Int -> Bool
divisivel20 x = if length (filter (== True) var) == 20
                then True
                else False
                where l = map (x `mod`) [1..20] 
                      var =  map (== 0) l
main = do 
          print(foldr1 lcm [1..20])