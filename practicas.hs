--TEMA 1:
--Ejercicio 1:
data ServicioPublico = Electricidad | Gas | Agua | Internet deriving (Eq, Show)
type Importancia = String
importanciaServicio:: ServicioPublico -> Importancia
importanciaServicio Agua = "Extremadamente importante"
importanciaServicio Electricidad = "Muy importante"
importanciaServicio Gas = "Importante"
importanciaServicio Internet = "Poco importante"

--Ejercicio 2:
type NombrePersona = String
type Monto = Int
data ServiciosDeuda = AgregaDeuda ServicioPublico NombrePersona Monto ServiciosDeuda | Ninguna deriving (Eq, Show)
tengoDeuda:: ServiciosDeuda -> ServicioPublico -> NombrePersona -> Monto
tengoDeuda Ninguna a b = 0
tengoDeuda (AgregaDeuda s p m x) a b |((a == s) && (b == p)) = m + tengoDeuda x a b
                                     |otherwise = tengoDeuda x a b

--TEMA 2:
--Ejercicio 1:
data Deporte = Futbol | Basket | Tenis | Valorant | Dota2 deriving (Eq, Show)
type MinJugadores = Int
minimaCantidad:: Deporte -> MinJugadores
minimaCantidad Futbol = 22
minimaCantidad Basket = 10
minimaCantidad Tenis = 2
minimaCantidad Valorant = 1
minimaCantidad Dota2 = 3

--Ejercicio 2:
data PracticoDeporte = AgregaDeporte Deporte NombrePersona PracticoDeporte | Ninguno deriving (Eq, Show)
deporte:: PracticoDeporte -> Deporte -> NombrePersona -> Bool
deporte Ninguno a b = False
deporte (AgregaDeporte d p x) a b |((a == d) && (b == p)) = True
                                  |otherwise = False || deporte x a b

--Ejercicio 3:
type EquipoFavorito = String
data ListaAsoc a b = Vacia | Nodo a b (ListaAsoc a b) deriving Show
agregaEquipoFavorito:: ListaAsoc Deporte EquipoFavorito -> Deporte -> EquipoFavorito -> ListaAsoc Deporte EquipoFavorito
agregaEquipoFavorito l a b = (Nodo a b (l))

--TEMA 3:
--Ejercicio 1:
data LugarALimpiar = Cocina | Habitacion | Comedor | Baño deriving (Eq, Show)
type Fiaca = Int
cantidadDeFiaca:: LugarALimpiar -> Fiaca
cantidadDeFiaca Habitacion = 0
cantidadDeFiaca Comedor = 1
cantidadDeFiaca Baño = 2
cantidadDeFiaca Cocina = 5

--Ejercicio 2:
data HayQueLimpiar = AgregaTarea LugarALimpiar NombrePersona HayQueLimpiar | Ningunas deriving (Eq, Show)
tocaLimpiar:: HayQueLimpiar -> LugarALimpiar -> NombrePersona -> Bool
tocaLimpiar Ningunas a b = False
tocaLimpiar (AgregaTarea l p x) a b |((a == l) && (b == p)) = True
                                    |otherwise = False || tocaLimpiar x a b

--Ejercicio 3:
type Tiempo = Int
agregaLA:: ListaAsoc LugarALimpiar Tiempo -> LugarALimpiar -> Tiempo -> ListaAsoc LugarALimpiar Tiempo
agregaLA l a b = (Nodo a b (l))

--TEMA A (2022):
--Ejercicio 1:
data Forma = Piedra | Papel | Tijera
igual::Forma -> Forma -> Bool
igual (Piedra) (Piedra) = True
igual (Tijera) (Tijera) = True
igual (Papel) (Papel) = True
igual otherwise = False

--a
le_gana:: Forma -> Forma -> Bool
le_gana Piedra Tijera = True
le_gana Piedra Piedra = False
le_gana Piedra Papel = False
le_gana Papel Piedra = True
le_gana Papel Papel = False
le_gana Papel Tijera = False
le_gana Tijera Papel = True
le_gana Tijera Tijera = False
le_gana Tijera Piedra = False

--b 
type Nombre = String
data Jugador = Mano Nombre Forma
ganador:: Jugador -> Jugador -> Maybe Nombre
ganador (Mano n f) (Mano m g) |((le_gana f g) == True) = Just n
                              |((le_gana g f) == True) = Just m
                              |otherwise = Nothing

--Ejercicio 2:
quien_jugo:: Forma -> [Jugador] -> [Nombre]
quien_jugo f [] = []
quien_jugo f ((Mano a b):xs) |((igual f b) == True) = a: quien_jugo f (xs)
                             |otherwise = quien_jugo f (xs)

