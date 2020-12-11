--numordenados.hs : leer 3 numeros y escribirlos en orden

main = do
  putStr "Primer valor: "
  linea <- getLine
  let a = read linea :: Int
  putStr "Segundo valor: "
  linea <- getLine
  let b = read linea :: Int
  putStr "Tercer valor: "
  linea <- getLine
  let c = read linea :: Int
  let max = mayor a b c
  let min = menor a b c
  let med = mid a b c min max
  putStr . show $ min
  putStr ", "
  putStr . show $  med
  putStr ", "
  print max

mayor a b c
  |a>=b && a>=c = a
  |b>=a && b>=c = b
  |c>=a && c>=b = c

menor a b c
  |a<=b && a<=c = a
  |b<=a && b<=c = b
  |c<=a && c<=b = c

mid a b c min max
  |(b==min && c==max)||(c==min && b==max) = a
  |(a==min && c==max)||(c==min && a==max) = b
  |(a==min && b==max)||(b==min && a==max) = c 
