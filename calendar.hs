--calendar.hs

main = do
  putStr "Año: "
  linea <- getLine
  let a = read linea :: Int
  putStr "Mes: "
  linea <- getLine
  let m = read linea ::Int
  let mon = mes m :: String

  putStrLn $ "Mes de " ++  mon ++ " del año " ++ (show a)

  if esbisiesto a == True
    then putStrLn "El año es bisiesto"
    else putStrLn "El año no es bisiesto"
  let n = a -1
  let a1 = a - 1
  let añoDia = sumaA a1 n
  let diaAño = diaSem (añoDia)
  putStrLn $ "El año " ++ (show a) ++ " empieza en dia " ++ diaAño
  let diaAñoMes =
        if m == 1 
          then añoDia
          else ((añoDia + (sumaMes a (m-1))) `mod` 7)
  putStrLn $ "El mes de " ++ mon ++ " del año " ++ (show a) ++ " empieza en dia " ++ (diaSem diaAñoMes)
  let calendario = imprimeCal m diaAñoMes a
  putStrLn $ "\n"++mon ++" "++(show a)  
  putStrLn calendario
  putStrLn ""

esbisiesto n = or [and [((n `mod` 4)==0),((n `mod` 100)/=0)],and[((n`mod`4)==0),((n`mod`100)==0),((n`mod`400)==0)]]

imprimeCal m dm a
  | esbisiesto a == True && m == 2 = "Do Lu Ma Mi Ju Vi Sa\n"++
			  	     imprDia dm 0 1 md2
                                       where
                                         md = diasMes m
                                         md2 = md + 1
imprimeCal m dm a = "Do Lu Ma Mi Ju Vi Sa\n"++
                    imprDia dm 0 1 md
                      where
                        md = diasMes m
imprDia dm n d md
  | d>md = ""
  | d<=md && dm/=n = "   " ++ (if n==6 then "\n" else "")++
                     imprDia dm ((n+1)`mod`7) d md

imprDia dm n d md
  | d<=md && dm==n = (if d<10 then " "++(show d)++" " else (show d)++" ")++
                     (if n==6 then "\n" else "")++
                     imprDia ((dm+1)`mod`7) ((n+1)`mod`7) (d+1) md

sumaA a n
  | n<0 = 0 
sumaA a n
  | n==0 = 1
sumaA a n
  | esbisiesto a == True = ((d1+2) `mod` 7)
    where
      d1 = sumaA (a-1) (n-1)
sumaA a n
  | esbisiesto a == False = ((d1+1) `mod` 7)
    where
      d1 = sumaA (a-1) (n-1)

sumaMes a m
  |m<=0 = 0
sumaMes a m
  | and [esbisiesto a,m==2] = ((n1+n2+1) `mod` 7)
    where
      n1 = sumaMes a (m-1)
      n2 = diasMes m
sumaMes a m = ((n1+n2) `mod` 7)
  where
    n1 = sumaMes a (m-1)
    n2 = diasMes m

diaSem n
  |n==0 = "domingo"
  |n==1 = "lunes"
  |n==2 = "martes"
  |n==3 = "miercoles"
  |n==4 = "jueves"
  |n==5 = "viernes"
  |n==6 = "sabado"

diasMes n
  |n==1 = 31
  |n==2 = 28
  |n==3 = 31
  |n==4 = 30
  |n==5 = 31
  |n==6 = 30
  |n==7 = 31
  |n==8 = 31
  |n==9 = 30
  |n==10 = 31
  |n==11 = 30
  |n==12 = 31

mes n
  |n==1 = "enero"
  |n==2 = "febrero"
  |n==3 = "marzo"
  |n==4 = "abril"
  |n==5 = "mayo"
  |n==6 = "junio"
  |n==7 = "julio"
  |n==8 = "agosto"
  |n==9 = "septiembre"
  |n==10 = "octubre"
  |n==11 = "noviembre"
  |n==12 = "diciembre"
  |otherwise = "-"
