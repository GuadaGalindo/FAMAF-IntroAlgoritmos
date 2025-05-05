--PROYECTO 1:

--Ejercicio 1.a:
esCero:: Int -> Bool
esCero x = x==0

--Ejercicio 1.b:
esPositivo:: Int -> Bool
esPositivo x = x>0

--Ejercicio 1.c:
esVocal:: Char -> Bool
esVocal x = x=='a' || x=='e' || x=='i' || x=='o' || x=='u'

--Ejercicio 2.a:
paratodo:: [Bool] -> Bool
paratodo [] = True
paratodo (x:xs) |(x==True) = True && paratodo xs
                |(x==False) = False

--Ejercicio 2.b:
sumatoria:: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

--Ejercicio 2.c:
productoria:: [Int] -> Int
productoria [] = 1
productoria (x:xs) = x * productoria xs

--Ejercicio 2.d:
factorial:: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

--Ejercicio 2.e:
promedio:: [Int] -> Int
promedio (xs) = div (sumatoria xs) (length xs)

--Ejercicio 3:
pertenece:: Int -> [Int] -> Bool
pertenece y [] = False
pertenece y (x:xs) |(y==x) = True
                   |(y/=x) = False || pertenece y (xs)

--Ejercicio 4.a:
paratodo':: [a] -> (a -> Bool) -> Bool
paratodo' [] f = True
paratodo' (x:xs) f = (f x) && paratodo' xs f

--Ejercicio 4.b:
existe':: [a] -> (a -> Bool) -> Bool
existe' [] f = False
existe' (x:xs) f = (f x) || existe' xs f

--Ejercicio 4.c:
sumatoria':: [a] -> (a -> Int) -> Int
sumatoria' [] f = 0
sumatoria' (x:xs) f = (f x) + sumatoria' xs f

--Ejercicio 4.d:
productoria':: [a] -> (a -> Int) -> Int
productoria' [] f = 1
productoria' (x:xs) f = (f x) * productoria' xs f

--Ejercicio 5:
paratodo''::[Bool] -> Bool
paratodo'' xs = paratodo' xs id

--Ejercicio 6.a:
par:: Int -> Bool
par x = (mod x 2 == 0)
todosPares:: [Int] -> Bool
todosPares xs = paratodo' (xs) par

--Ejercicio 6.b:
multiplo:: Int -> Int -> Bool
multiplo x y = (mod y x == 0)
hayMultiplo:: Int -> [Int] -> Bool
hayMultiplo x (ys) = existe' ys (multiplo x)

--Ejercicio 6.c:
cuadrados:: Int -> Int
cuadrados x = x^2
sumaCuadrados:: Int -> Int
sumaCuadrados x = sumatoria' [0..x] cuadrados

--Ejercicio 6.d:
factorial':: Int -> Int
factorial' x = productoria' [1..x] id

--Ejercicio 6.e:
multiplicaPares:: [Int] -> Int
multiplicaPares xs = productoria' (filter par xs) id

--Ejercicio 7:
-- map: es una función que aplica otra función f a cada elemento de una lista xs, devolviendo asi una nueva lista.
-- filter: toma una lista y un predicado, y devuleve una nueva lista con aquellos elementos que satisfacen el predicado.
-- map succ [1, -4, 6, 2, -8] es equivalente a [2, -3, 7, 3, -7]
-- filter esPositivo [1, -4, 6, 2, -8] es equivalente a [1, 6, 2]

--Ejercicio 8.a:
duplica:: [Int] -> [Int]
duplica [] = []
duplica (x:xs) = (x*2): duplica xs

--Ejercicio 8.b:
por2:: Int -> Int
por2 x = x*2
duplica':: [Int] -> [Int]
duplica' xs = map por2 xs

--Ejercicio 9.a:
soloPares:: [Int] -> [Int]
soloPares [] = []
soloPares (x:xs) |(mod x 2 == 0) = x:soloPares xs
                 |(mod x 2 /= 0) = soloPares xs

--Ejercicio 9.b:
soloPares':: [Int] -> [Int]
soloPares' xs = filter par xs

--Ejercicio 10.a:
primIgualesA:: Eq a => a -> [a] -> [a]
primIgualesA x [] = []
primIgualesA x (y:ys) |(x==y) = y: primIgualesA x (ys)
                      |otherwise = []

--Ejercicio 10.b:
primIgualesA':: Eq a => a -> [a] -> [a]
primIgualesA' y xs = takeWhile (==y) xs

--Ejercicio 11.a:
primIguales:: Eq a => [a] -> [a]
primIguales [] = []
primIguales [x] = [x]
primIguales (x:y:xs) |(x==y) = x: primIguales (y:xs)
                     |otherwise = x:[]

--Ejercicio 11.b:
primIguales':: Eq a => [a] -> [a]
primIguales' [] = []
primIguales' (x:xs) = primIgualesA' x (x:xs)

