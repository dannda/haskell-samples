--mergesort.hs

main = do
  putStr "Lista a ordenar: "
  linea <- getLine
  let entrada = read linea :: [Int]
      salida = mergesort entrada
  putStrLn $ "Lista ordenada: "++(show salida)

mergesort [] = []
mergesort [x] = [x]
mergesort ent = merge (mergesort l1) (mergesort l2)
  where
    (l1,l2) = dividir ent

dividir :: [x] -> ([x],[x])
dividir [] = ([],[])
dividir [x] = ([x],[])
dividir (h1:h2:t) = ((h1:t1),(h2:t2))
  where
    (t1,t2) = dividir t

merge [] x = x
merge x [] = x
merge (h1:t1) (h2:t2)
  |h1 <= h2 = (h1:(merge t1 (h2:t2)))
  |otherwise = (h2:(merge (h1:t1) t2))
