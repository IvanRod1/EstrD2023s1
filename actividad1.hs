            {-Funciones Numeros enteros-}
--A
sucesor :: Int -> Int
sucesor n = n + 1

--B
sumar :: Int -> Int -> Int
sumar n m = n + m

--C
divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto n m = (div n m, mod n m)

--D
maxDelPar :: (Int,Int) -> Int
maxDelPar (n, m) = if (n > m)
                    then n
                    else m

{-Ejemplos-}

--sumar ((sucesor (maxDelPar (divisionYResto 16 2)))) 1
--sucesor (sumar (maxDelPar (divisionYResto 4 2)) (sumar 5 2))


                {-Funciones Tipos enumerativos-}
{-1-}
data Dir = Norte | Este | Sur | Oeste
     deriving Show
--A
opuesto :: Dir -> Dir
opuesto d = 
    case d of
     Norte -> Sur
     Sur   -> Norte
     Este  -> Oeste
     Oeste -> Este
{-opuesto Norte = Sur
opuesto Sur   = Norte
opuesto Este  = Oeste
opuesto Oeste = Este-}

--B
iguales :: Dir -> Dir -> Bool
iguales Norte Norte = True
iguales Sur Sur = True
iguales Este Este = True
iguales Oeste Oeste = True

--C
siguiente :: Dir -> Dir
siguiente Norte = Este
siguiente Este = Sur
siguiente Sur = Oeste
siguiente Oeste = Norte

{-2-}
data DiaDeSemana = Lunes|Martes|Miercoles|Jueves|Viernes|Sabado|Domingo
     deriving Show
--A
primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primeroYUltimoDia = (Lunes,Domingo) 

--B
empiezaConM :: DiaDeSemana -> Bool
empiezaConM Miercoles = True
empiezaConM Martes = True

--C
vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues Martes Lunes = True
vieneDespues Miercoles Martes = True
vieneDespues Jueves Miercoles = True
vieneDespues Viernes Jueves = True
vieneDespues Sabado Viernes = True
vieneDespues Domingo Sabado = True
vieneDespues Lunes Domingo = True 

--D
estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio Lunes = False
estaEnElMedio Domingo = False
estaEnElMedio Martes = True
estaEnElMedio Miercoles = True
estaEnElMedio Jueves = True
estaEnElMedio Viernes = True
estaEnElMedio Sabado = True

{-3-}
--A
negar :: Bool -> Bool
negar True = False
negar False = True

--B
implica :: Bool -> Bool -> Bool
implica True False = False
implica True True = True
implica False _ = True

--C
yTambien :: Bool -> Bool -> Bool
yTambien True True = True
yTambien _ _ = False

--D
oBien :: Bool -> Bool -> Bool
oBien True _ = True
oBien _ True = True
oBien _ _ = False


                {-Funciones Registros-}
data Persona = Persona String Int deriving Show
                       --Nombre Edad
yo = Persona "Ivan" 21
otro = Persona "Jesolo" 30
{-1-}
--A
nombre :: Persona -> String
nombre (Persona nombre edad) = nombre  

--B
edad :: Persona -> Int
edad (Persona nombre edad) = edad

--C
{-crecer :: Persona -> Persona
crecer (Persona nombre edad) = (Persona nombre (sumar edad 1))-}
crecer :: Persona -> Int
crecer (Persona nombre edad) = sumar edad 1

--D
cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre nuevoNombre (Persona nombre edad) = (Persona nuevoNombre edad) 

--E
esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra unaPersona otraPersona = edad unaPersona > edad otraPersona 

{-2-}

data Pokemon = Pokemon TipoDePokemon Int Entrenador deriving Show
data TipoDePokemon = Planta|Fuego|Agua deriving Show
data Entrenador = Entrenador String (Pokemon,Pokemon)  deriving Show

chorizord = Pokemon Fuego 100 ash
bulbasor = Pokemon Planta 150 ash
ash = Entrenador "Ash" (chorizord,bulbasor)

--A

tipo :: Pokemon -> TipoDePokemon
tipo (Pokemon tipoDePokemon _ _) = tipoDePokemon

superaA :: Pokemon -> Pokemon -> Bool
superaA Agua(unPokemon) Fuego(otroPokemon) = True
superaA Fuego(unPokemon) Planta(otroPokemon) = True
superaA Planta(unPokemon) Agua(otroPokemon) = True
superaA _ _ = False

esSuperior :: Pokemon -> Bool






















 




