----------------------------------------------------------------------------
---------------------------- GUIA 5 ----------------------------------------
----------------------------------------------------------------------------

--EJ 1--

--1
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
longitud :: [t] -> Integer
longitud [] = 0
longitud [a] = 1
longitud x = 1 + longitud (tail x)

--2
ultimo :: [t] -> t
ultimo (x:xs) = last xs

--3
primero :: [t] -> [t]
primero x = sacarPrimero (reverse x)

sacarPrimero :: [t] -> [t]
sacarPrimero (x:xs) = xs

--4
reverso :: [t] -> [t]
reverso = reverse




--------
--EJ 2--
--------

--1
pertenece :: (Eq t) => [t] -> t -> Bool
pertenece [] _ = False
pertenece (x:xs) t | x == t = True
                   | otherwise = pertenece xs t


--2
todosIguales :: [Int] -> Bool
todosIguales [] = False
todosIguales [a,b] = a == b
todosIguales (x:y:xs) | x == y = todosIguales (y:xs)
                      | otherwise = False

--3

todosDistintos :: [Int] -> Bool
todosDistintos [] = False
todosDistintos [a,b] = a /= b
todosDistintos (x:xs) | pertenece xs x = False
                    | otherwise = todosDistintos xs


--4
hayRepetidos :: [Int] -> Bool

hayRepetidos (x:xs) | pertenece xs x = True
                    | otherwise = hayRepetidos xs

--5
quitar :: (Eq t) => [t] -> t -> [t]
quitar [] _ = []

quitar (x:xs) t | x == t = xs
                | otherwise = x : quitar xs t

--6
quitarTodo :: (Eq t) => [t] -> t -> [t]
quitarTodo [] _ = []
quitarTodo (x:xs) t | x == t = quitarTodo xs t
                    | otherwise = x : quitarTodo xs t

--7
quitarRepetidos :: (Ord t) => [t] -> [t]
quitarRepetidos [] = []
quitarRepetidos (x:xs) | pertenece xs x = quitarRepetidos xs
                       | otherwise = x : quitarRepetidos xs

--8
mismosElementos :: (Ord t) => [t] -> [t] -> Bool
mismosElementos [] [] = True
mismosElementos x y |  pertenece yNoRep (head xNoRep) = mismosElementos y (tail x)
                    | otherwise = False
                where xNoRep = quitarRepetidos x
                      yNoRep = quitarRepetidos y
--------
--EJ 3--
--------
--1--

sumatoria :: [Integer] -> Integer
sumatoria [x] = x
sumatoria (x:xs) = x + sumatoria xs

--2--
productoria :: [Integer] -> Integer
productoria [x] = x
productoria (x:xs) = x * productoria xs


--3--
maximo :: [Integer] -> Integer

maximo [a,b] = max a b
maximo (x:y:xs) | x > y = maximo (x:xs)
                | otherwise = maximo (y:xs)


--4--

sumarN :: Integer -> [Integer] -> [Integer]

sumarN n [] = []
sumarN n [x] = [n + x]
sumarN n (x:xs) = (x + n) : sumarN n xs



--5--
sumarElPrimero :: [Integer] -> [Integer]
sumarElPrimero [x] = [2*x]
sumarElPrimero (x:xs) = sumarN x (x:xs)



--6--
sumarElUltimo :: [Integer] -> [Integer]

sumarElUltimo [x] = [2*x]
sumarElUltimo (x:xs) = sumarN (last (x:xs)) (x:xs)


--7--
pares :: [Integer] -> [Integer]

pares [] = []
pares (x:xs) | even x = x : pares xs
             | otherwise = pares xs


--8--
multiplosDeN :: Integer -> [Integer] -> [Integer]


multiplosDeN _ [] = []
multiplosDeN n (x:xs) | mod x n == 0 = x : multiplosDeN n xs
                      | otherwise =  multiplosDeN n xs


--9--
ordenarMayorAMenor :: [Integer] -> [Integer]

ordenarMayorAMenor [a] = [a]
ordenarMayorAMenor (x:xs) = mayor :  ordenarMayorAMenor listaSinMaximo
                where mayor = maximo (x:xs)
                      listaSinMaximo = quitar (x:xs) mayor


ordenar :: [Integer] -> [Integer]
ordenar xs = reverse (ordenarMayorAMenor xs)


--------
--EJ 4--
--------



--1--
sacarBlancosRepetidos :: [Char] -> [Char]

--EJ -> sacarBlancosRepetidos "hola  todo bien" -> "hola todo bien"
--EJ -> sacarBlancosRepetidos "asdasdasasd  cucucu  " -> "asdasdasasd cucucu"
sacarBlancosRepetidos [x] = [x]
sacarBlancosRepetidos (x1:x2:xs) | esBlanco x1 && esBlanco x2 = sacarBlancosRepetidos (x1:xs)
                                 | otherwise = x1 : sacarBlancosRepetidos (x2:xs)

esBlanco :: Char -> Bool
esBlanco x = x == ' '

--2--
contarPalabras :: [Char] -> Integer
contarPalabras [] = 0
contarPalabras [x] = 1
contarPalabras xs | head listaLimpia == ' ' = 1 + contarPalabras (tail listaLimpia)
                      | otherwise = contarPalabras (tail listaLimpia)
                        where listaLimpia = sacarBlancosRepetidos xs

--3--
palabras :: [Char] -> [[Char]]
palabras [] = []
palabras xs = primeraPalabra listaLimpia : palabras (sacarPrimeraPalabra listaLimpia)
      where listaLimpia = sacarBlancosRepetidos xs

primeraPalabra :: [Char] -> [Char]
primeraPalabra [] = []
primeraPalabra (x:xs) | x == ' ' = []
                      | otherwise = x : primeraPalabra xs

sacarPrimeraPalabra :: [Char] -> [Char]
sacarPrimeraPalabra [] = []
sacarPrimeraPalabra (x:xs) | x == ' ' = xs
                           | otherwise = sacarPrimeraPalabra xs


--4--
palabraMasLarga :: [Char] -> [Char]

palabraMasLarga xs = compararPalabras (palabras xs)

longitudPalabra :: [Char] -> Integer

longitudPalabra [] = 0
longitudPalabra (x:xs) = 1 + longitudPalabra xs


compararPalabras :: [[Char]] -> [Char]
compararPalabras [x] = x
compararPalabras (x:y:xs) | longitudPalabra x >= longitudPalabra y = compararPalabras (x:xs)
                          | otherwise = compararPalabras (y:xs)


--6--


aplanar :: [[Char]] -> [Char]
-- aplanar ["hola", "todo", "bien"] => "holatodobien"

aplanar [x] = x
aplanar (x:xs) = x ++ aplanar xs


--6--


aplanarConblancos :: [[Char]] -> [Char]
-- aplanarConblancos ["hola", "todo", "bien"] => "hola todo bien"

aplanarConblancos [x] = x
aplanarConblancos (x:xs) = x ++ " " ++ aplanarConblancos xs


--7--

aplanarConNBlancos :: [[Char]] -> Integer -> [Char]
aplanarConNBlancos [x] _ =  x
aplanarConNBlancos (x:xs) n = x ++ agregarNespacios n ++ aplanarConNBlancos xs n

agregarNespacios :: Integer -> [Char]
agregarNespacios 0 = ""
agregarNespacios 1 = " "
agregarNespacios n = " " ++ agregarNespacios (n-1)



--------
--EJ 5--
--------

--1--


-- EJ sumaAcumulada [1, 2, 3, 4, 5] => [1, 1+2, 1+2+3, 1+2+3+4, 1+2+3+4+5].
sumaAcumulada :: [Int] -> [Int]
sumaAcumulada xs = reverse ( sumaInvertida xs)

sumaInvertida :: [Int] -> [Int]
sumaInvertida [x] = [x]
sumaInvertida xs = sumarLista xs : sumaInvertida (init xs)


sumarLista :: [Int] -> Int
sumarLista [x] = x
sumarLista (x:xs) = x + sumarLista xs



--2--
descomponerEnPrimos :: [Integer] -> [[Integer]]
descomponerEnPrimos [a] = [factorizar a]
descomponerEnPrimos (x:xs) = factorizar x : descomponerEnPrimos xs


factorizar :: Integer -> [Integer]

factorizar n | esPrimo n = [n]
             | otherwise = menorDivisorN : factorizar (div n menorDivisorN)
             where menorDivisorN = menorDivisorDesde n 2

menorDivisorDesde :: Integer -> Integer -> Integer
menorDivisorDesde n k | mod n k == 0 = k
                      | otherwise = menorDivisorDesde n (k+1)

esPrimo :: Integer -> Bool
esPrimo n = cantDivisoresHasta n n == 2


cantDivisoresHasta :: Integer -> Integer -> Integer
cantDivisoresHasta n 1 = 1
cantDivisoresHasta n k | mod n k == 0 = 1 + cantDivisoresHasta n (k-1)
                       | otherwise = cantDivisoresHasta n (k-1)