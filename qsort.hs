--qsort.hs : ordenar una lista

main = do
  putStr "Lista a ordenar: "
  linea <- getLine
  let entrada = read linea :: [Int]
      salida = qs entrada
  putStrLn $ "La lista ordenada es: " ++ (show salida)

qs []=[]
qs (x:resto) = qs [y | y <- resto, y<=x] ++ [x] ++ qs [ z | z<-resto, z>x]
