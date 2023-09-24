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


--EJ 10


--a
-- problema f1(n:entero):naturales {
-- requiere : {n natural}
-- asegura : {res = la suma de todas
-- las potencias de 2 desde 0 hasta n }
-- }
f1 :: Integer -> Integer
f1 0 = 1
f1 x = (2^x) + f1 (x-1)


-- b
-- problema f2(q:real, n:entero): Real {
-- requiere : {true}
-- asegura : {res = sumatoria de q^i desde i=1 hasta n}
-- }
f2 :: Float -> Integer -> Float
f2 0 _ = 0
f2 q n | q <= 0 || n < 1 = 0
       | otherwise = (q^n) + f2 q (n-1)


-- c
-- problema f3(n:natural, q:real):real
-- requiere : {true}
-- asegura : {res = sumatoria de q^i desde i=1 hasta 2n}
-- }

f3 :: Float -> Integer -> Float
f3 0 _ = 0
f3 _ 0 = 1
f3 q n = (q^(2*n)) + (q^((2*n) -1 )) + f3 q (n-1)


-- d
-- problema f4(n:natural, q:real):real
-- requiere : {true}
-- asegura : {res = sumatoria de q^i desde i=n hasta 2n}
-- }

f4 :: Float -> Integer -> Float
f4 0 _ = 0
f4 _ 0 = 1
f4 q n = (q^(2*n)) +  (q^((2*n) -1 )) + f4 q (n-1)

-- EJ 11


-- a
factorial :: Float -> Float
factorial 1 = 1
factorial n = n * factorial (n-1)

eAprox :: Float -> Float

eAprox 0 = 1
eAprox i  = (1.0 / factorial i ) + eAprox (i-1)


-- b
eConst :: Float
eConst = eAprox 10

-- EJ 12
-- problema raizDe2Aprox(i:integer):float {
-- requiere {i >0}
-- asegura {res = aproximación de raiz de 2 con i términos dada por la fórmula  1/(An-1)}
-- }

raizDe2Aprox :: Integer -> Float
raizDe2Aprox 1 = 1
raizDe2Aprox i = 1/raizDe2Aprox (i-1)





--EJ 13
primerSumatoria :: Integer -> Integer -> Integer

primerSumatoria _ 0 = 0
primerSumatoria n m = (n ^ m) + primerSumatoria n (m-1)


sumaDoble :: Integer -> Integer -> Integer
sumaDoble 1 m = m
sumaDoble n m = primerSumatoria n m + sumaDoble (n-1) m


--EJ 14
sumaPotencias :: Integer ->Integer ->Integer ->Integer
sumaPotencias q n m = 1
