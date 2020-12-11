--cosx.hs

import System.Environment

main = do
  args <- getArgs
  let x = read (args!!0)::Float
  let n = read (args!!1)::Float
  let cos = coseno x n
  putStrLn $ "Coseno de "++(show x)++" con "++(show n)++" terminos es: "++(show cos)

coseno 0 n = 1
coseno x 0 = 0
coseno x n =
  ((xalan (-1) (n+1)) * ((xalan (x) ((2*n)-2)) / (product[1..(2*n-2)] ))) + coseno x (n-1)

xalan x 0 = 1
xalan x n = (xalan x (n-1))*x
