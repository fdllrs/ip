f :: Integer -> Integer

-- ejercicio 1a
f 1 = 8
f 4 = 131
f 16 = 16
f x = -1


-- ejercicio 1b
g :: Integer -> Integer

g 8 = 16
g 16 = 4
g 131 = 1
g x = -1

h :: Integer -> Integer
h x = f (g x)
k :: Integer -> Integer
k x = g (f x)



-- ejercicio 2
-- a)
-- problema Absoluto(x:Float): Float {
-- requere: {True}
-- Asegura {res = x para todo x >= 0 y será -x para x < 0}
--}
absoluto :: Float -> Float

absoluto x | x >= 0 = x
           | x < 0 = -x

-- b)
-- problema maximoAbsoluto(x:Int, y:Int): Int {
-- requere: {x /= y}
-- Asegura: {res = Absoluto(x) si x > y }
-- Asegura: {res = Absoluto(y) si x < y}
--}

maximoAbsoluto :: Float -> Float -> Float

maximoAbsoluto x y | absoluto x > absoluto y = absoluto x
                   | otherwise = absoluto y

-- c)
-- problema maximoDe3(x:Int, y:Int, z:Int): Int {
-- requere: {x /= y /= z}
-- Asegura: {res = x si x es el mayor de todos }
-- Asegura: {res = y si y es el mayor de todos }
-- Asegura: {res = z si z es el mayor de todos }
--}

maximoDe3 :: Ord a => a -> a -> a -> a
maximoDe3 x y z | x > y && x > z =  x
                | y > x && y > z =  y
                | z > x && z > y =  z


-- d)
-- problema algunoEs0(x:Int, y:Int): Bool {
-- requere: {True}
-- Asegura: {res = True si x es 0 o si y = 0 }
-- Asegura: {res = False si ninguno es 0 }
--}
algunoEs0PM :: Integer -> Integer -> Bool
algunoEs0 :: Integer -> Integer -> Bool

algunoEs0PM 0 _ = True
algunoEs0PM _ 0 = True
algunoEs0PM _ _ = False

algunoEs0 x y | x == 0 || y == 0 = True
              | otherwise = False


-- e)
-- problema ambosSon0(x:Int, y:Int): Bool {
-- requere: {True}
-- Asegura: {res = True si x e y son 0 }
-- Asegura: {res = False si y es 0 }
--}
ambosSon0PM :: Integer -> Integer -> Bool
ambosSon0 :: Integer -> Integer -> Bool


ambosSon0PM 0 0 = True
ambosSon0PM _ _ = False

ambosSon0 x y | x == 0 && y == 0 = True
              | otherwise = False

-- f)
-- problema mismoIntervalo(x:Int, y:Int): Bool {
-- requere: {True}
-- Asegura: {res = True si x e y pertenecen al mismo intervalo de los siguientes: (−∞, 3], (3, 7] y (7, ∞) }
-- Asegura: {res = False si x e y no pertenecen al mismo intervalo  de los siguientes: (−∞, 3], (3, 7] y (7, ∞) }
--}

mismoIntervalo :: Integer -> Integer -> Bool


mismoIntervalo x y | (x <= 3) && (y <= 3) = True
                   | (x > 3) && (x <= 7) && (y > 3) && (y <= 7) = True
                | (x > 7) && (y > 7) = True
                   | otherwise = False





-- g)
-- problema sumaDistintos(x:Int, y:Int, z:Int): Int {
-- requere: {True}
-- Asegura: {res = la suma de los parámetros que no se repitan}
--}


sumaDistintos :: Integer -> Integer -> Integer -> Integer
sumaDistintos x y z | x == y = x + z
                    | (x == z) || (y == z) = x + y
                    | otherwise = x + y + z



-- h)
-- problema esMultiploDe(x:Int, y:Int): Int {
-- requere: {x e y naturales}
-- Asegura: {res = True si x es múltiplo de y}
--}

esMultiploDe :: Integer -> Integer -> Bool
esMultiploDe x y | remainder == 0 = True
                 | otherwise = False
                 where (_, remainder) = divMod x y



-- i)
-- problema digitoUnidades(x:Int): Int {
-- requere: {x sea natural}
-- Asegura: {res será el dígito de las unidades de x}
--}



digitoUnidades :: Integer -> Integer

digitoUnidades x | x < 10 = x
                 | otherwise = remainder
                 where (_,remainder) = divMod x 10



-- j)
-- problema digitoDecenas(x:Int): Int {
-- requere: {x sea natural}
-- Asegura: {res será el dígito de las decenas de x}
--}

digitoDecenas :: Integer -> Integer
digitoDecenas x = mod (div x 10) 10





--------------------------------------------------------------
-- Ejercicio 3
--------------------------------------------------------------
estanRelacionados :: Float -> Float -> Bool
estanRelacionados a b | (a == 0) || (b == 0) = False
                      | a * a + a * b * k == 0.0 = True
                      | otherwise = False
                      where k = -a / b
                        

--------------------------------------------------------------
-- Ejercicio 4
--------------------------------------------------------------

-- a)
-- problema prodInt(x, y:Seq[float]): Seq[float]) {
-- requere: {x e y tengan 2 elementos}
-- Asegura: {res será el producto punto entre x e y}
--}

prodInt :: (Float , Float) -> (Float , Float) -> (Float, Float)

prodInt (x1, x2) (y1, y2) = (x1 + y1, x2 + y2)



-- b)
-- problema todoMenor(x, y:Seq[float]): Seq[float]) {
-- requere: {x e y tengan 2 elementos}
-- Asegura: {res <-> todos los elementos de x son menores que su correspondiente elemento en y}
--}

todosMenores :: (Float, Float) -> (Float, Float) -> Bool
todosMenores (x1, x2) (y1, y2) = (x1 < y1) && (x2 < y2)


-- c)
-- problema distanciaPuntos(x, y:Seq[float]): float) {
-- requere: {x e y tengan 2 elementos}
-- Asegura: {la distancia entre x e y en el plano R^2}
--}

distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos (x1, x2) (y1, y2) = sqrt (coord1^2 + coord2^2)
                                    where coord1 = absoluto (x1 - y1)
                                          coord2 = absoluto (x2 - y2)


-- d)
-- problema sumaTerna(x:Seq[float]): float) {
-- requere: {x tenga 3 elementos}
-- Asegura: {res será la suma de todos los elementos de x}
--}



sumaTerna :: (Float, Float, Float) -> Float
sumaTerna (x1, x2, x3) = x1 + x2 + x3




-- e)
-- problema sumarSoloMultiplos(x: Seq[Int], y:Int): Integer) {
-- requere: {y > 0}
-- requere: {y = 3}

-- Asegura: {res = la suma de los elementos de la terna que
-- son multiplos de y}
--}

-- sumarSoloMultiplos :: (Integer, Integer, Integer) -> Integer -> Integer

-- sumarSoloMultiplos (x1, x2, x3) y = 
--    if esMultiploDe x1 y 
--       then concat x1 multiplos
--    if esMultiploDe x2 y 
--       then concat x2 multiplos
--    if esMultiploDe x3 y 
--       then concat x3 multiplos

-- f)
-- problema posPrimerPar(x: Seq[Int]): Integer) {
-- requere: {x = 3}
-- Asegura: {res = posición en la secuencia del primer número que sea par}
-- Asegura: {res = 4 si no hay números pares en la secuencia}
--}


posPrimerPar :: (Integer, Integer, Integer) -> Integer
posPrimerPar (x1, x2, x3) | even x1 = 1
                          | even x2 = 2
                          | even x3 = 3
                          | otherwise = 4

-- g)
-- problema crearPar(x, y: any): seq[x,y]) {
-- requere: {True}
-- Asegura: {res = una secuencia que contiene los elementos x e y en orden}
--}

crearPar :: t0 -> t1 -> (t0,t1)
crearPar x y = (x,y)

-- h)
-- problema invertir(seq[x,y : any]): seq[x,y : any]) {
-- requere: {True}
-- Asegura: {res = una secuencia que contiene los elementos x e y en orden inverso}
--}

invertir :: (t0, t1) -> (t1,t0)
invertir (x, y) = (y,x)


--------------------------------------------------------------
-- Ejercicio 5
--------------------------------------------------------------


-- problema todosMenores3 ((n1,n2,n3) : Z×Z×Z) : Bool {
-- requiere: {True}
-- asegura: {(res = true) ↔ ((f (n1) > g(n1)) ∧(f (n2) > g(n2)) ∧(f (n3) > g(n3))))}
-- }
-- problema f (n: Z) : Z{
-- requiere: {True}
-- asegura: {(n ≤7 →res = n2) ∧(n > 7 →res = 2n −1)}
-- }
-- problema g (n: Z) : Z{
-- requiere: {True}
-- asegura: {Si n es un n ́umero par, entonces res = n/2, en caso contrario, res = 3n + 1}
-- }






todosMenores3 :: (Integer, Integer, Integer) ->Bool
todosMenores3 (x1,x2,x3) = (f1 x1 > g1 x1) && (f1 x2 > g1 x2) && (f1 x3 > g1 x3)




f1 :: Integer -> Integer
f1 x | x <= 7 = x * x
     | otherwise = 2 * x - 1  



g1 :: Integer -> Integer

g1 x | even x = div x 2
     | otherwise = x * 3 + 1



--------------------------------------------------------------
-- Ejercicio 6
--------------------------------------------------------------

-- problema bisiesto (año: Z) : Bool {
-- requiere: {True}
-- asegura: {res = f alse ↔año no es multiplo de 4 o
--  año es multiplo de 100 pero no de 400}
-- }


bisiesto :: Integer -> Bool
bisiesto x = mod x 4 == 0 || (mod x 100 /= 0 && (mod x 400 == 0))

-- está mal la especificación


--------------------------------------------------------------
-- Ejercicio 7
--------------------------------------------------------------

-- distanciaManhattan:: (Float, Float, Float) -> (Float, Float, Float) ->Float
-- problema distanciaManhattan (p : R×R×R, q : R×R×R) : R{
-- requiere: {True}
-- asegura: {res = ∑2
-- i=0 |pi −qi|}
-- }


distanciaManhattan:: (Float, Float, Float) -> (Float, Float, Float) ->Float

distanciaManhattan (x1,x2,x3) (y1,y2,y3)= absoluto (x1-y1) + absoluto (x2-y2) + absoluto (x3-y3)  



--------------------------------------------------------------
-- Ejercicio 8
--------------------------------------------------------------



-- problema comparar (a:Z, b:Z) : Z{
-- requiere: {T rue}
-- asegura: {(res = 1 ↔sumaU ltimosDosDigitos(a) < sumaU ltimosDosDigitos(b))}
-- asegura: {(res = −1 ↔sumaU ltimosDosDigitos(a) > sumaU ltimosDosDigitos(b))}
-- asegura: {(res = 0 ↔sumaU ltimosDosDigitos(a) = sumaU ltimosDosDigitos(b)))}
-- }
-- problema sumaUltimosDosDigitos (x: Z) : Z{
-- requiere: {T rue}
-- asegura: {res = (x m ́od 10) + (b(x/10)c m ́od 10)}
-- }


