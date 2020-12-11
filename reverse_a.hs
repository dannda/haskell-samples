--reverse_a : escribe la última línea a primera línea, cada línea de último caracter a primer caracter

main = do
  putStr "Archivo a invertir: "
  nombre <- getLine
  contenido <- readFile nombre
  let lineas = lines contenido
  imprimeL lineas
  putStrLn ""

imprimeL [] = do
  putStrLn ""
imprimeL (x:resto) = do
  imprimeL resto
  imprimeC x
  
imprimeC [] = do
  putStrLn ""
imprimeC (x:resto) = do
  imprimeC resto
  putChar x
