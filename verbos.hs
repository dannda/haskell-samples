--verbos.hs

main = do
  putStr "Da el verbo en infinitivo: "
  verbo <- getLine
  let verb = read verbo ::String
  let n = length verbo
      term = drop (n-2) verbo
  putStrLn $ "La terminacion es: "++term
  putStrLn $ "¿Es verbo? "++(show(esVerbo term))
  let base = take (n-2) verbo

  if (esVerbo term)==True
    then do
      contenido <- readFile "irregulares.txt"
      let lineas = lines contenido
      enIrregulares verbo lineas base term
    else putStrLn""

esVerbo term= or[term=="ar",term=="er",term=="ir"]

enIrregulares verbo [] base term = do
  putStrLn "No en archivo"
  conjugar base term
  putStr "¿Es correcto? Si/No "
  linea <- getLine
  if or[stringEq linea "si",stringEq linea "Si",stringEq linea "SI",stringEq linea "s"]
    then putStrLn ""
    else do
      putStrLn "Conjugue el verbo:"
      appendFile "irregulares.txt" (verbo++" ")
      putStr "yo "
      conj <- getLine
      appendFile "irregulares.txt" (conj++" ")
      putStr "tú "
      conj <- getLine
      appendFile "irregulares.txt" (conj++" ")
      putStr "él/ella "
      conj <- getLine
      appendFile "irregulares.txt" (conj++" ")
      putStr "nosotros "
      conj <- getLine
      appendFile "irregulares.txt" (conj++" ")
      putStr "ustedes "
      conj <- getLine
      appendFile "irregulares.txt" (conj++" ")
      putStr "ellos "
      conj <- getLine
      appendFile "irregulares.txt" (conj++"\n")
--      appendFile "irregulares.txt" "\n"

enIrregulares verbo (x:resto) base term=do
  let w=words x ::[String]
  if (stringEq (w!!0) verbo)==True
    then do
      putStrLn "En archivo"
      imprimeL w
    else do
      enIrregulares verbo resto base term

stringEq [] [] = True
stringEq (x:xs) (y:ys) = and[(x==y),(stringEq xs ys)]
stringEq x y = False

imprimeL [] = putStrLn ""
imprimeL (x:resto) = do
  putStrLn x
  imprimeL resto

conjugar verbo term
  |term=="ar" = conjugaAr verbo
  |term=="er" = conjugaEr verbo
  |term=="ir" = conjugaIr verbo

conjugaAr verbo = do
  putStrLn $ verbo++"o"
  putStrLn $ verbo++"as"
  putStrLn $ verbo++"a"
  putStrLn $ verbo++"amos"
  putStrLn $ verbo++"an"
  putStrLn $ verbo++"an"

conjugaEr verbo = do
  putStrLn $ verbo++"o"
  putStrLn $ verbo++"es"
  putStrLn $ verbo++"e"
  putStrLn $ verbo++"emos"
  putStrLn $ verbo++"en"
  putStrLn $ verbo++"en"

conjugaIr verbo = do
  putStrLn $ verbo++"o"
  putStrLn $ verbo++"es"
  putStrLn $ verbo++"e"
  putStrLn $ verbo++"imos"
  putStrLn $ verbo++"en"
  putStrLn $ verbo++"en"
