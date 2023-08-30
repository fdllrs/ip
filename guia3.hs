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

h x = f (g x)
k x = g (f x)



-- ejercicio 2
-- a)
-- problema Absoluto(x:Int): Int {
-- requere: {True}
-- Asegura {res = x para todo x >= 0 y será -x para x < 0}
--}
absoluto :: Integer -> Integer

absoluto x | x >= 0 = x
           | x < 0 = -x

-- b)
-- problema maximoAbsoluto(x:Int, y:Int): Int {
-- requere: {x /= y}
-- Asegura: {res = Absoluto(x) si x > y }
-- Asegura: {res = Absoluto(y) si x < y}
--}

maximoAbsoluto :: Integer -> Integer -> Integer

maximoAbsoluto x y | absoluto x > absoluto y = absoluto x
                   | otherwise = absoluto y

-- c)
-- problema maximoDe3(x:Int, y:Int, z:Int): Int {
-- requere: {x /= y /= z}
-- Asegura: {res = x si x es el mayor de todos }
-- Asegura: {res = y si y es el mayor de todos }
-- Asegura: {res = z si z es el mayor de todos }
--}

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
-- requere: {True}
-- Asegura: {res será el dígito de las unidades de x}
--}





















