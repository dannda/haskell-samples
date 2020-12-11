--reinas.hs

main = do
  putStr "Numero de reinas: "
  linea <- getLine
  let n = read linea ::Int
  putStrLn $ (show n)
  let lista = [0..(n-1)]
  putStrLn $ (show lista)

checa lista n = do
  checaR2 (ren)
  putStr ren
  imprimeR ren n
    where 
      ren = permuta lista

checaR2 (x:resto) = do
  let n = 1
  itera x resto

permuta [] = []
permuta (x:resto) = (insertaTodas x (permuta resto))

insertaTodas x [] = [x]
insertaTodas x (y:resto) = (x:y:resto)
insertaTodas x (y:resto) = (insertaTodas x resto)
