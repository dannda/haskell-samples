-- misycan.hs : misioneros y canibales

main = do
  putStrLn "Estado inicial "
  putStr "Misioneros: "
  linea <- getLine
  let m = read linea ::Int
  putStr "Canibales: "
  linea <- getLine
  let c = read linea ::Int
  putStr "Bote: "
  linea <- getLine
  let b = read linea ::Int
  sol (m,c,b)

sol (m,c,b) = busca (m,c,b) ([(m,c,b)])

busca (m,c,b) path
  |(esSolucion (m,c,b))==True = imprimeSol path
busca (m,c,b) epath = do
  putStr ""

intenta (m,c,1) = (nm,nc,0)
		  where
		    (nm,nc) = movOF (m,c,2)
intenta (m,c,1) = (nm,nc,0)
		  where
		    (nm,nc) = movOF (m,c,1)
intenta (m,c,0) = (nm,nc,1)
		  where
		    (nm,nc) = movFO (m,c,2)
intenta (m,c,0) = (nm,nc,1)
		  where
		    (nm,nc) = movFO (m,c,1)

movOF (m,c,0) = (m,c)
movOF (m,c,n)
  |(m-1)>=0 = movOF ((m-1),c,(n-1)) 
movOF (m.c.n)
  |(c-1)>=0 = movOF (m,(c-1),(n-1))

movFO (m,c,0) = (m,c)
movFO (m,c,n)
  |(m+1)<=3 = movFO ((m+1),c,(n-1))

esSolucion (m,c,b) 
  |(m,c,b)==(0,0,0) = True
  |otherwise = False

imprimeSol [] = putStr ""
imprimeSol ((m,c,b):resto) = do
  imprimeSol resto
  let m1 = 3 - m
      c1 = 3 - c
      n1 = m
      n2 = n1 + c
      n3 = 6 - n2
  imprimeN n3
  imprimeB b
  imprimeM m1 0
  imprimeC c1 0
  putStrLn ""

imprimeM 0 n = putStr ""
imprimeM m n = do
  putStr "M"
  imprimeM (m-1) (n+1)

imprimeC 0 n = putStr ""
imprimeC c n = do
  putStr "C"
  imprimeC (c-1) (n+1)

imprimeN 0 = putStr ""
imprimeN n = do
  putStr " "
  imprimeN (n-1)

imprimeB 1 = putStr "|-__-     |"
imprimeB 0 = putStr "|     -__-|"
