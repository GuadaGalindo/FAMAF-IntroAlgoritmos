--PROEYCTO 2:

--Ejercico 1.a:
data Carrera = Matematica | Fisica | Computacion | Astronomia deriving (Eq, Show)

--Ejercico 1.b:
titulo:: Carrera -> String
titulo Matematica = "Licenciatura en Matematica"
titulo Fisica = "Licenciatura en Fisica"
titulo Computacion = "Licenciatura en ciencias de la Computacion"
titulo Astronomia = "Licenciatura en Astronomia"

--Ejercico 1.c:
data NotaBasica = Do | Re | Mi | Fa | Sol | La | Si deriving (Eq, Ord, Show)

--Ejercico 1.d:
cifradoAmericano:: NotaBasica -> Char
cifradoAmericano Do = 'C'
cifradoAmericano Re = 'D'
cifradoAmericano Mi = 'E'
cifradoAmericano Fa = 'F'
cifradoAmericano Sol = 'G'
cifradoAmericano La = 'A'
cifradoAmericano Si = 'B'

--Ejercicio 3.a:
minimoElemento:: Ord a => [a] -> a
minimoElemento [x] = x
minimoElemento (x:xs) = min x (minimoElemento xs)

--Ejercicio 3.b:
minimoElemento':: (Ord a, Bounded a) => [a] -> a
minimoElemento' [] = maxBound
minimoElemento' (x:xs) = min x (minimoElemento' xs)

--Ejercicio 4.a:
type Ingreso = Int
data Cargo = Titular | Asociado | Adjunto | Asistente | Auxiliar deriving (Eq, Show)
data Area = Administrativa | Ensenanza | Economia | Postgrado deriving (Show, Eq)
data Persona = Decane | Docente Cargo | NoDocente Area | Estudiante Carrera Ingreso deriving (Show, Eq)

--Ejercicio 4.b:
-- Docente :: Cargo -> Persona

--Ejercicio 4.c:
cuantos_doc:: [Persona] -> Cargo -> Int
cuantos_doc [] c = 0
cuantos_doc ((Docente x):xs) c |(c == x) = 1 + cuantos_doc (xs) c
                               |(c /= x) = cuantos_doc (xs) c
cuantos_doc (y:xs) c = cuantos_doc (xs) c

--Ejercicio 4.d:
cargoDocente:: Cargo -> Persona -> Bool
cargoDocente d (Docente c) = c==d
cuantos_doc':: [Persona] -> Cargo -> Int
cuantos_doc' xs d = length (filter (cargoDocente d) xs)

--Ejercicio 5.a:
data Alteracion = Bemol | Sostenido | Natural deriving Eq
data NotaMusical = Nota NotaBasica Alteracion 
sonido:: NotaBasica -> Int
sonido Do = 1
sonido Re = 3
sonido Mi = 5
sonido Fa = 6
sonido Sol = 8
sonido La = 10
sonido Si = 12

--Ejercicio 5.b:
sonidoCromatico:: NotaMusical -> Int
sonidoCromatico (Nota x y) |(y == Bemol) = (sonido x) - 1
                           |(y == Sostenido) = (sonido x) +1
                           |(y == Natural) = (sonido x)

--Ejercicio 5.c:
instance Eq NotaMusical
  where
  y == x = sonidoCromatico y == sonidoCromatico x

--Ejercicio 5.d:
instance Ord NotaMusical 
  where
  y <= x = sonidoCromatico y <= sonidoCromatico x

--Ejercicio 6.a:
primerElemento:: [a] -> Maybe a
primerElemento [] = Nothing
primerElemento (x:xs) = Just x

--Ejercicio 7.a.1:
data Cola = VaciaC | Encolada Persona Cola deriving Show
atender:: Cola -> Maybe Cola
atender VaciaC = Nothing
atender (Encolada p c) = Just c

--Ejercicio 7.a.2:
encolar:: Persona -> Cola -> Cola
encolar p VaciaC = Encolada p VaciaC
encolar p (Encolada p' c) = Encolada p' (encolar p c)

--Ejercicio 7.a.3:
busca:: Cola -> Cargo -> Maybe Persona
busca VaciaC x = Nothing
busca (Encolada (Docente d) c) x |(d == x) = Just (Docente d)
                                 |(d /= x) = busca c x
busca (Encolada p c) x = busca c x

--Ejercicio 7.b:
--La tipo cola se parece al tipo lista.

--Ejercicio 8.a:
data ListaAsoc a b = Vacia | Nodo a b (ListaAsoc a b) deriving Show
type GuiaTelefonica = ListaAsoc String Int

--Ejercicio 8.b.1:
la_long:: ListaAsoc a b -> Int
la_long Vacia = 0
la_long (Nodo x y (l)) = 1 + la_long l

--Ejercicio 8.b.2:
la_concat:: ListaAsoc a b -> ListaAsoc a b -> ListaAsoc a b
la_concat Vacia l = l
la_concat (Nodo x y (l')) l = (Nodo x y (la_concat l' l))

--Ejercicio 8.b.3:
la_agregar:: ListaAsoc a b -> a -> b -> ListaAsoc a b
la_agregar l a b = (Nodo a b (l))

--Ejercicio 8.b.4:
la_pares:: ListaAsoc a b -> [(a,b)]
la_pares Vacia = []
la_pares (Nodo x y (l)) = [(x,y)] ++ la_pares l

--Ejercicio 8.b.5:
la_busca:: Eq a => ListaAsoc a b -> a -> Maybe b
la_busca Vacia z = Nothing
la_busca (Nodo x y (l)) z |(x /= z) = la_busca l x
                          |(x == z) = Just y

--Ejercicio 8.b.6:
la_borrar:: Eq a => a -> ListaAsoc a b -> ListaAsoc a b
la_borrar z Vacia = Vacia
la_borrar z (Nodo x y (l)) |(x /= z) = (Nodo x y (la_borrar z l))
                           |(x == z) = la_borrar z l