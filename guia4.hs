-----------------------------------------------------------------------------------------------
--------------------------------------------GUIA 4---------------------------------------------
-----------------------------------------------------------------------------------------------

-- EJ 1
fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)


-- EJ 2
parteEntera :: Float -> Integer
parteEntera x | 0 <= x && x < 1 = 0
              | 0 >= x && x > -1 = 0
              | x > 0 = 1 +  parteEntera(x - 1)
              | x < 0 = -1 +  parteEntera(x + 1)


-- 3)
-- problema esDivisible(x,y:Int): Bool {
-- requiere : {x,y naturales}
-- asegura: {res <-> x es divisible por y}
--}

-- esDivisible x y |



--EJ 6
sumaDigitos :: Integer -> Integer
sumaDigitos x | x < 10 = x
              | otherwise = ultimoDigito x + sumaDigitos (sacarUltimoDigito x)
 



-- EJ 7
todosDigitosIguales :: Integer -> Bool
todosDigitosIguales n | n <= 9 = True
                      | ultimoDigito n == anteUltimoDigito = todosDigitosIguales(sacarUltimoDigito n)
                      | otherwise = False
                      where anteUltimoDigito =  ultimoDigito (sacarUltimoDigito 10)

ultimoDigito :: Integer -> Integer
ultimoDigito n = mod n 10

sacarUltimoDigito :: Integer -> Integer
sacarUltimoDigito n = div n 10



-- EJ 8
cantDigitos :: Integer -> Integer
cantDigitos u | u < 10 = 1
              | otherwise = 1 + cantDigitos( sacarUltimoDigito u )


iesimoDigito :: Integer -> Integer -> Integer
iesimoDigito n i | i == cantDigitos n = ultimoDigito n
                 | otherwise = iesimoDigito (sacarUltimoDigito n) i