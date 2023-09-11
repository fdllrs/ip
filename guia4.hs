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
              | x > 0 = 1 +  parteEntera (x - 1)
              | x < 0 = -1 +  parteEntera (x + 1)


-- 3)
-- problema esDivisible(x,y:Int): Bool {
-- requiere : {x,y naturales}
-- asegura: {res <-> x es divisible por y}
--}
esDivisible :: Integer -> Integer -> Bool

esDivisible _ 1 = True
esDivisible x y | x < y = False
                | x == y = True
                | otherwise = esDivisible (x-y) y

-- 4)
-- problema sumaImpares(x:Int): Int {
-- requiere : {x natural y mayor que 0}
-- asegura: {res = la suma desde 1 hasta x de todos los x impares}
--}

sumaImpares :: Integer -> Integer
sumaImpares 1 = 1
sumaImpares x | x <= 0 = 0
              |otherwise = (2*x-1) + sumaImpares (x-1)

--EJ 5
medioFactorial :: Integer -> Integer
medioFactorial 0 = 1
medioFactorial x | x <= 0 = 1
                 |otherwise = x * medioFactorial (x-2)


--EJ 6
sumaDigitos :: Integer -> Integer
sumaDigitos x | x < 10 = x
              | otherwise = ultimoDigito x + sumaDigitos (sacarUltimoDigito x)




-- EJ 7
todosDigitosIguales :: Integer -> Bool
todosDigitosIguales n | n <= 9 = True
                      | ultimoDigito n == anteUltimoDigito = todosDigitosIguales (sacarUltimoDigito n)
                      | otherwise = False
                      where anteUltimoDigito =  ultimoDigito (sacarUltimoDigito 10)

ultimoDigito :: Integer -> Integer
ultimoDigito n = mod n 10

sacarUltimoDigito :: Integer -> Integer
sacarUltimoDigito n = div n 10



-- EJ 8
cantDigitos :: Integer -> Integer
cantDigitos u | u < 10 = 1
              | otherwise = 1 + cantDigitos ( sacarUltimoDigito u )


iesimoDigito :: Integer -> Integer -> Integer
iesimoDigito n i | i == cantDigitos n = ultimoDigito n
                 | otherwise = iesimoDigito (sacarUltimoDigito n) i

-- 9)
-- problema esCapicua(x:Int): Bool {
-- requiere : {x natural}
-- asegura: {res <-> el número x es capicúa}
--}

esCapicua :: Integer -> Bool
esCapicua x | x < 10 = True
            | ultimoDigito x == primerDigito x = esCapicua (sacarUltPrim x)
            | otherwise = False

primerDigito :: Integer -> Integer
primerDigito x | x < 10 = x
               | otherwise = primerDigito(div x 10)

sacarUltPrim :: Integer -> Integer
sacarUltPrim x | x <  10 = x
               | otherwise = sacarUltimoDigito (mod x (10 ^ (cantDigitos x -1)))