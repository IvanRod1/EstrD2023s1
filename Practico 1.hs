---Ivan Rodriguez

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
--maxDelPar (divisionYResto (sumar 45 5) (sucesor 4))
--sumar (maxDelPar(divisionYResto 36 6)) (sucesor 3)



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
iguales _ _ = False

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
empiezaConM _ = False

--C
vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
{-vieneDespues Martes Lunes = True
vieneDespues Miercoles Martes = True
vieneDespues Jueves Miercoles = True
vieneDespues Viernes Jueves = True
vieneDespues Sabado Viernes = True
vieneDespues Domingo Sabado = True
vieneDespues Lunes Domingo = True 
vieneDespues _ _ = False-}
vieneDespues a b = (numeroDelDiaDeUnaSemana a) > (numeroDelDiaDeUnaSemana b)

numeroDelDiaDeUnaSemana :: DiaDeSemana -> Int
numeroDelDiaDeUnaSemana dia =
          case dia of
          Lunes -> 1
          Martes -> 2
          Miercoles -> 3
          Jueves -> 4
          Viernes -> 5
          Sabado -> 6
          Domingo -> 7

--D
estaEnElMedio :: DiaDeSemana -> Bool
{-estaEnElMedio Martes = True
estaEnElMedio Miercoles = True
estaEnElMedio Jueves = True
estaEnElMedio Viernes = True
estaEnElMedio Sabado = True
estaEnElMedio _ = False-}

estaEnElMedio Lunes = False
estaEnElMedio Domingo = False
estaEnElMedio _ = True

{-3-}
--A
negar :: Bool -> Bool
negar True = False
negar _ = True

--B

implica :: Bool -> Bool -> Bool
implica p q = oBien (negar p) q

--C
yTambien :: Bool -> Bool -> Bool
yTambien p q =
     case p of
          True -> q 
          _ -> False

--D
oBien :: Bool -> Bool -> Bool
oBien p q =
     case p of
          True ->True
          _ -> q

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
crecer :: Persona -> Persona
crecer (Persona nombre edad) = (Persona nombre (sumar edad 1))

--D
cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre nuevoNombre (Persona nombre edad) = (Persona nuevoNombre edad) 

--E
esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra unaPersona otraPersona = edad unaPersona > edad otraPersona

--F
laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor persona1 persona2 = if esMayorQueLaOtra persona1 persona2
                                 then persona1
                                 else persona2

{-2-}

data Pokemon = Pokemon TipoDePokemon Int deriving Show
data TipoDePokemon = Planta|Fuego|Agua deriving Show
data Entrenador = Entrenador String Pokemon Pokemon  deriving Show

chorizord = Pokemon Fuego 140 
bulbasor = Pokemon Planta 150
balastoid= Pokemon Agua 180
chakarita = Pokemon Planta 110

ash = Entrenador "Ash" chorizord bulbasor
jamemes = Entrenador "jamemes" balastoid chakarita

--A

tipo :: Pokemon -> TipoDePokemon
tipo (Pokemon tipoDePokemon _) = tipoDePokemon 

pokemon1 :: Entrenador -> Pokemon
pokemon1 (Entrenador _ pokemon1 _) = pokemon1

pokemon2 :: Entrenador -> Pokemon 
pokemon2 (Entrenador _ _ pokemon2) = pokemon2

superaA :: Pokemon -> Pokemon -> Bool
superaA unPokemon otroPokemon = tipoDePokemonLeGanaA (tipo(unPokemon)) (tipo(otroPokemon))

tipoDePokemonLeGanaA :: TipoDePokemon -> TipoDePokemon -> Bool
tipoDePokemonLeGanaA Agua Fuego = True
tipoDePokemonLeGanaA Fuego Planta = True
tipoDePokemonLeGanaA Planta Agua = True
tipoDePokemonLeGanaA _ _ = False


--B

elPokemon_esDeTipo_ :: Pokemon -> TipoDePokemon -> Int
elPokemon_esDeTipo_ unPokemon unTipoDePokemon = if comparadorDeTiposDePokemon (tipo(unPokemon)) ((unTipoDePokemon))
                                                then 1
                                                else 0 

comparadorDeTiposDePokemon :: TipoDePokemon -> TipoDePokemon-> Bool
comparadorDeTiposDePokemon Agua Agua = True
comparadorDeTiposDePokemon Fuego Fuego = True
comparadorDeTiposDePokemon Planta Planta = True
comparadorDeTiposDePokemon _ _ = False

cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonDe tipoDePokemon unEntrenador = elPokemon_esDeTipo_ (pokemon1(unEntrenador)) tipoDePokemon + elPokemon_esDeTipo_ (pokemon2(unEntrenador)) tipoDePokemon

--C

juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemon (unEntrenador,otroEntrenador) = listaPokemonDe_ unEntrenador ++ listaPokemonDe_ otroEntrenador

listaPokemonDe_ :: Entrenador -> [Pokemon]
listaPokemonDe_ unEntrenador = --(pokemon1(unEntrenador) : []) ++ (pokemon2(unEntrenador) : [])
                                [pokemon1(unEntrenador)] ++ [pokemon2(unEntrenador)]

                              {-Funciones polimórficas-}
{-1-}

--A
loMismo :: a -> a
loMismo a = a

--B
siempreSiete :: a -> Int
siempreSiete a = 7

--C
swap :: (a,b) -> (b, a)
swap (a,b) = (b,a)

                           {-Pattern matching sobre listas-}

{-1-}

--A
estaVacia :: [a] -> Bool 
estaVacia [] = True
estaVacia _ = False    

--B
elPrimero :: [a] -> a
elPrimero (a:_) = a

--C
sinElPrimero :: [a] -> [a]
sinElPrimero(_:a) = a

--D
splitHead :: [a] -> (a, [a])
splitHead a = (elPrimero a, sinElPrimero a)




















 



