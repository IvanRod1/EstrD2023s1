                        {- Recursión sobre listas -}
--1
sumatoria :: [Int] -> Int
sumatoria [] = 0 
sumatoria (x:xs) = x + sumatoria xs

--2
longitud :: [a] -> Int
longitud [] = 0
longitud (_:xs) = 1 + longitud xs

--3
sucesores :: [Int] -> [Int]
sucesores [] = []
sucesores (x:xs) = x + 1 : sucesores xs

--4
conjuncion :: [Bool] -> Bool
conjuncion [] = True
conjuncion (b:bs) = b && conjuncion bs 
                            {-if estaVacia bs
                            then b
                            else last bs-}

estaVacia :: [a] -> Bool 
estaVacia [] = True
estaVacia _ = False
--5

disyuncion :: [Bool] -> Bool
disyuncion [] = False
disyuncion (b:bs) = b || disyuncion bs

--6

aplanar :: [[a]] -> [a]
aplanar [] = []
aplanar(l:ls) = l ++ aplanar ls

--7
pertenece :: Eq a => a -> [a] -> Bool
pertenece  e [] = False
pertenece e (x:xs) = e == x  || pertenece e xs

--8
apariciones :: Eq a => a -> [a] -> Int
apariciones e [] = 0
apariciones e (x:xs) = if e == x
                       then 1 + apariciones e xs
                       else apariciones e xs

--9
losMenoresA :: Int -> [Int] -> [Int]
losMenoresA b [] = []
losMenoresA b (x:xs) = if x < b 
                       then x : losMenoresA b xs
                       else losMenoresA b xs

--10
lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA b [] = []
lasDeLongitudMayorA b (l:ls) = if longitud l > b
                               then l : lasDeLongitudMayorA b ls
                               else lasDeLongitudMayorA b ls

--11
agregarAlFinal :: [a] -> a -> [a]
--agregarAlFinal [] b = b : []
agregarAlFinal a b = agregar a [b] 

--12
agregar :: [a] -> [a] -> [a]
agregar [] b = b
agregar (x:xs) b = x : agregar xs b

--13
reversa :: [a] -> [a]
reversa [] = []
reversa(x:xs) =  reversa xs ++ [x] 

-- 14
elMayorDe :: Int -> Int -> Int
elMayorDe a b = if a > b
                then a
                else b

zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos a [] = a
zipMaximos [] b = b
zipMaximos (x:xs) (y:ys) = elMayorDe x y : zipMaximos xs ys 

{-zip1 :: [a] -> [b] -> [(a,b)]
zip1[] _ = []
zip1 (x:xs) [] = []
zip1 (x:xs) (y:ys) = (x,y) : zip1 xs ys-}

-- 15
minimoEntre :: Ord a => a -> a -> a
minimoEntre a b = if a < b 
                  then a 
                  else b

elMinimo :: Ord a => [a] -> a
elMinimo [] = error "No hay nada"
elMinimo[a] = a
elMinimo(x:xs) = minimoEntre x (elMinimo xs)

                            {-Recursión sobre números-}
--1
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

--2
cuentaRegresiva :: Int -> [Int]
cuentaRegresiva 0 = []
cuentaRegresiva n = if n < 1
                    then []
                    else n : cuentaRegresiva (n - 1)


--3
repetir :: Int -> a -> [a]
repetir 0 _ = []
repetir n a = a : repetir (n - 1) a 

--4
losPrimeros :: Int -> [a] -> [a]
losPrimeros _ [] = []
losPrimeros 0 _ = []
losPrimeros n (x:xs) = x : losPrimeros (n - 1) xs  

--5
sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros _ [] = []
sinLosPrimeros 0 (_:xs) = xs
sinLosPrimeros n (x:xs) = if (n == 0)
                          then x : sinLosPrimeros n xs
                          else sinLosPrimeros (n - 1) xs

                                    {-Registros-}
data Persona = P String Int deriving Show
pepe = P "pepe" 32
pola = P "polaco" 23
kike = P "KIKE" 56
uri = P "Uri" 0
{-1-}

edad :: Persona -> Int
edad (P _ edad) = edad
--A
mayoresA :: Int -> [Persona] -> [Persona]
mayoresA _ [] = []
mayoresA n (x:xs) = if edad x > n
                    then x : mayoresA n xs
                    else mayoresA n xs

--B
promedioEdad :: [Persona] -> Int
--Precondición: la lista al menos posee una persona.
promedioEdad [] = 0
promedioEdad a  = div (sumatoriaDeEdades a) (longitud a)

sumatoriaDeEdades::[Persona] -> Int
sumatoriaDeEdades [] = 0
sumatoriaDeEdades(x:xs) = edad x + sumatoriaDeEdades xs

--C
elMasViejo :: [Persona] -> Persona
elMasViejo [] = error "No hay persona"
elMasViejo [a] = a
elMasViejo(x:xs) = maximaEdadEntre x (elMasViejo xs)

maximaEdadEntre :: Persona -> Persona -> Persona
maximaEdadEntre p1 p2 = if edad p1 > edad p2 
                        then p1 
                        else p2

{-2-}

data TipoDePokemon = Agua | Fuego | Planta deriving Show
data Pokemon = ConsPokemon TipoDePokemon Int deriving Show
data Entrenador = ConsEntrenador String [Pokemon] deriving Show

balastoid = ConsPokemon Agua 100
chakarita = ConsPokemon Planta 90
chorizord = ConsPokemon Fuego 110
pochinki = ConsPokemon Agua 1000


ash = ConsEntrenador "Ash" [balastoid,chakarita,chorizord,pochinki]
jamemes = ConsEntrenador "Jamemes" [chorizord,chorizord,chakarita]

--A
cantPokemon :: Entrenador -> Int
cantPokemon (ConsEntrenador _ pokemones) = longitud pokemones

--B

cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonDe tp ep = longitud (pokemonesDelTipo_En_ tp (listPokemon ep))


pokemonesDelTipo_En_:: TipoDePokemon -> [Pokemon] -> [Pokemon]
pokemonesDelTipo_En_ tp [] = []
pokemonesDelTipo_En_ tp (x:xs) = if comparadorDeTiposDePokemon tp (tipo x)
                                 then x : pokemonesDelTipo_En_ tp xs
                                 else pokemonesDelTipo_En_ tp xs


listPokemon :: Entrenador -> [Pokemon]
listPokemon (ConsEntrenador _ pokemones) = pokemones

tipo :: Pokemon -> TipoDePokemon
tipo (ConsPokemon tipoDePokemon _) = tipoDePokemon 


comparadorDeTiposDePokemon :: TipoDePokemon -> TipoDePokemon-> Bool
comparadorDeTiposDePokemon Agua Agua = True
comparadorDeTiposDePokemon Fuego Fuego = True
comparadorDeTiposDePokemon Planta Planta = True
comparadorDeTiposDePokemon _ _ = False

--C

cuantosDeTipo_De_LeGananATodosLosDe_ :: TipoDePokemon -> Entrenador -> Entrenador -> Int
--Dado un tipo de pokemon, y dos entrenadores, nos dice la cantidad de pokemones del tipo dado del primer entrenador que ganaron contra todos los pokemones del segundo entrenador
cuantosDeTipo_De_LeGananATodosLosDe_ tp ep1 ep2 = longitud (pokemonesDelTipo_En_ tp (pokemonesDe_QueGanaronContraTodosLosPokemonesDe_ (listPokemon ep1) (listPokemon ep2)))

pokemonesDe_QueGanaronContraTodosLosPokemonesDe_:: [Pokemon] -> [Pokemon] -> [Pokemon]
-- Dado una dos listas de pokemon, devuelve una lista de pokemon de la primera lista de los cuales ganaron todas las peleas contra los pokemon de la segunda lista de pokemon
pokemonesDe_QueGanaronContraTodosLosPokemonesDe_ [] _ = []
pokemonesDe_QueGanaronContraTodosLosPokemonesDe_ _ [] = []
pokemonesDe_QueGanaronContraTodosLosPokemonesDe_ (x:xs) lp = if pokemonContraListaDePokemones x lp
                                                                 then x : pokemonesDe_QueGanaronContraTodosLosPokemonesDe_ xs lp
                                                                 else pokemonesDe_QueGanaronContraTodosLosPokemonesDe_ xs lp 
    
                                                        {-if tipoDePokemonLeGanaA (tipo x) (tipo y)       <- Codigo malo
                                                        then x : pokemonesDe_QueGanaronContraPokemonesDe_ xs ys 
                                                        else pokemonesDe_QueGanaronContraPokemonesDe_ xs ys-}

--funcion para verificar si un pokemon le gana a todos los de una lista de pokemon
--que deberia devolver?
pokemonContraListaDePokemones :: Pokemon -> [Pokemon] -> Bool
--Funcion para indicar si el pokemon dado, le gana a todos los pokemon de la lista de pokemones
pokemonContraListaDePokemones p [] = True
pokemonContraListaDePokemones  p (x:xs) = tipoDePokemonLeGanaA (tipo p) (tipo x) && pokemonContraListaDePokemones p xs

                                          {-if tipoDePokemonLeGanaA (tipo p) (tipo x)
                                          then pokemonContraListaDePokemones p xs
                                          else tipoDePokemonLeGanaA (tipo p) (tipo x)-}

tipoDePokemonLeGanaA :: TipoDePokemon -> TipoDePokemon -> Bool
tipoDePokemonLeGanaA Agua Fuego = True
tipoDePokemonLeGanaA Fuego Planta = True
tipoDePokemonLeGanaA Planta Agua = True
tipoDePokemonLeGanaA _ _ = False

{-pokemonesDelTipo_En_:: TipoDePokemon -> [Pokemon] -> [Pokemon]
pokemonesDelTipo_En_ tp [] = []
pokemonesDelTipo_En_ tp (x:xs) = if comparadorDeTiposDePokemon (tp) (tipo x)
                                 then x : pokemonesDelTipo_En_ tp xs
                                 else pokemonesDelTipo_En_ tp xs
Lo pongo aca para recordar -}

--D
esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon ep = (tienePokemonTipo Agua (listPokemon ep)) && (tienePokemonTipo Fuego (listPokemon ep)) && (tienePokemonTipo Planta (listPokemon ep))

tienePokemonTipo :: TipoDePokemon -> [Pokemon] -> Bool
tienePokemonTipo tp [] = False
tienePokemonTipo tp (x:xs) = comparadorDeTiposDePokemon (tipo x) tp || tienePokemonTipo tp xs 
                            {-if comparadorDeTiposDePokemon (tipo x) tp
                              then True
                              else tienePokemonTipo tp xs-}

------------ no tener en cuenta
{-tienePokemonTipo1 :: TipoDePokemon -> [Pokemon] -> [Bool]
tienePokemonTipo1 tp [] = []
tienePokemonTipo1 tp (x:xs) = comparadorDeTiposDePokemon (tipo x) tp : tienePokemonTipo tp xs-}

{-3-}

data Seniority = Junior | SemiSenior | Senior deriving Show
data Proyecto = ConsProyecto String deriving Show
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto deriving Show
data Empresa = ConsEmpresa [Rol] deriving Show

pacopaco = ConsProyecto "pacopaco"
eskaip = ConsProyecto "eskaip"
zomboid = ConsProyecto "zomboid"

juanDeveloper = Developer Senior pacopaco
mikeManagement = Management SemiSenior eskaip
joseDeveloper = Developer SemiSenior pacopaco
polaManagement = Management Senior eskaip
miloDeveloper = Developer Junior pacopaco
solDeveloper = Developer Senior zomboid

stean = ConsEmpresa [juanDeveloper,mikeManagement,joseDeveloper,polaManagement,miloDeveloper,solDeveloper]


--A
--Dada una empresa denota la lista de proyectos en los que trabaja, sin elementos repetidos.
proyectos :: Empresa -> [Proyecto]
proyectos e = rolesDeLosProyectosDeLaEmpresa (rolesEmpresa e)


rolesEmpresa :: Empresa -> [Rol]
-- Funcion observadora para obtener la lista de roles de la empresa dada
rolesEmpresa (ConsEmpresa rol) = rol

rolesDeLosProyectosDeLaEmpresa :: [Rol] -> [Proyecto]
-- Dado una lista de roles, devuelve una lista de proyectos, que los mismos pertenecen a los roles dados
rolesDeLosProyectosDeLaEmpresa [] = []
rolesDeLosProyectosDeLaEmpresa (x:xs) = if perteneceAlRol (proyectoDeUnRol x) xs
                                        then rolesDeLosProyectosDeLaEmpresa xs
                                        else proyectoDeUnRol(x) : rolesDeLosProyectosDeLaEmpresa xs
                                        

perteneceAlRol :: Proyecto -> [Rol] -> Bool 
-- Indica si el proyecto dado, pertenece a algun rol dentro de la lista
perteneceAlRol p [] = False
perteneceAlRol p (x:xs) = nombreProyecto p == nombreProyecto (proyectoDeUnRol x) || perteneceAlRol p xs
    
                        {-if ((nombreProyecto p) == nombreProyecto (proyectoDeUnRol x))
                          then True      -- <- miedo al bool 
                          else perteneceAlRol p xs -}

nombreProyecto :: Proyecto -> String
nombreProyecto (ConsProyecto s) = s

proyectoDeUnRol :: Rol -> Proyecto
-- Denota un proyecto del rol dado
proyectoDeUnRol rol =
    case rol of 
        Developer _ _-> proyectoDeveloper rol
        Management _ _-> proyectoManagement rol 

proyectoDeveloper :: Rol -> Proyecto
--Funcion observadora para obtener el proyecto de un rol Developer
proyectoDeveloper (Developer _ p) = p

proyectoManagement :: Rol -> Proyecto
--Funcion observadora para obtener el proyecto de un rol Management
proyectoManagement (Management _ p) = p 

--B
losDevSenior :: Empresa -> [Proyecto] -> Int
-- Dado una empresa y una lista de proyectos, devuelve la cantidad de Seniors que hay en la empresa dada trabajando en alguno de los proyectos dados
losDevSenior e [] = 0
losDevSenior e lp = cantidadSeniorEnListaDeRoles (rolesDondeLosProyectos_EstanPresentes lp (rolesEmpresa e))
    
    
cantidadSeniorEnListaDeRoles :: [Rol] -> Int
-- Dado una lista de roles, devuelve la cantidad de roles cuyo management sea Senior
cantidadSeniorEnListaDeRoles [] = 0
cantidadSeniorEnListaDeRoles (x:xs) = if hayDevSeniorEn x
                                      then 1 + cantidadSeniorEnListaDeRoles xs
                                      else cantidadSeniorEnListaDeRoles xs

hayDevSeniorEn :: Rol -> Bool
-- Indica si el seniority de un rol dado, es Senior
hayDevSeniorEn p =
    case seniorityDeUnRol p of
        Senior -> True
        Junior -> False
        SemiSenior -> False

       

estaElProyectoEnLaListaDeRoles :: Proyecto -> [Rol] -> Rol
--Dado un proyecto y una lista de royes, devuelve un rol cuyo proyecto sea el proyecto dado
estaElProyectoEnLaListaDeRoles p [] = error "El Proyecto no esta en la lista de roles"
estaElProyectoEnLaListaDeRoles p (x:xs) = if (nombreProyecto p) == nombreProyecto(proyectoDeUnRol x)
                                          then x
                                          else estaElProyectoEnLaListaDeRoles p xs

rolesDondeLosProyectos_EstanPresentes :: [Proyecto] -> [Rol] -> [Rol]
--rolesDondeLosProyectos_EstanPresentes (proyectos stean) (rolesEmpresa stean)
--Dado una lista de proyectos y una lista de roles, devuelve una lista de roles cuyo proyectos sean lo que aparecen en la lista de proyectos
rolesDondeLosProyectos_EstanPresentes [] [] = []
rolesDondeLosProyectos_EstanPresentes (x:xs) [] = []
rolesDondeLosProyectos_EstanPresentes [] (_:ys) = []
rolesDondeLosProyectos_EstanPresentes (x:xs) (_:ys) = estaElProyectoEnLaListaDeRoles x ys : rolesDondeLosProyectos_EstanPresentes xs ys

seniorityDeUnRol :: Rol -> Seniority
-- Funcion que sirve para obtener el Seniority de cualquier tipo de rol (Management, Developer)
seniorityDeUnRol r =
    case r of
        Management _ _ -> seniorityManagement r  
        Developer _ _ -> seniorityDeveloper r

seniorityDeveloper :: Rol -> Seniority
--Funcion observadora para obtener el Seniority de un rol tipo Developer
seniorityDeveloper (Developer s _) = s

seniorityManagement :: Rol -> Seniority
--Funcion observadora para obtener el Seniority de un rol tipo Management
seniorityManagement (Management s _) = s

--C
cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
--Indica la cantidad de empleados que trabajan en alguno de los proyectos dados.
cantQueTrabajanEn [] e = 0
cantQueTrabajanEn (x:xs) e = longitud(rolesImplicadosEnElProyecto x (rolesEmpresa e)) + cantQueTrabajanEn xs e 

rolesImplicadosEnElProyecto :: Proyecto -> [Rol] -> [Rol]
--rolesImplicadosEnElProyecto pacopaco (rolesEmpresa stean)
--Esta funcion devuelve una lista de roles que estan presentes en el proyecto dado
rolesImplicadosEnElProyecto p [] = []
rolesImplicadosEnElProyecto p (x:xs) = if nombreProyecto p == nombreProyecto(proyectoDeUnRol x)
                                       then x : rolesImplicadosEnElProyecto p xs
                                       else rolesImplicadosEnElProyecto p xs

--D
asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
--Devuelve una lista de pares que representa a los proyectos (sin repetir) junto con su cantidad de personas involucradas.
asignadosPorProyecto e = proyectosConSuCantidadDeEmpleados (proyectos e) e

proyectosConSuCantidadDeEmpleados :: [Proyecto] -> Empresa -> [(Proyecto,Int)]
-- Dado una lista de proyectos y una empresa, devuelve una lista de tuplas las cuales contienen los proyectos con la cantidad de roles que hay en cada proyecto
-- proyectosConSuCantidadDeEmpleados (proyectos stean) (rolesEmpresa stean)
proyectosConSuCantidadDeEmpleados [] _ = []
proyectosConSuCantidadDeEmpleados (x:xs) e = proyectoConSuCantidadDeEmpleados x e : proyectosConSuCantidadDeEmpleados xs e

--proyectosConSuCantidadDeEmpleados(x:xs) lr = (x,longitud(rolesImplicadosEnElProyecto x lr)) : proyectosConSuCantidadDeEmpleados xs lr <- Viejo

proyectoConSuCantidadDeEmpleados :: Proyecto -> Empresa -> (Proyecto,Int)
--Funcion que dado un proyecto y una empresa, devuelve una tupla con el proyecto dado y la cantidad de roles que forman parte del proyecto
proyectoConSuCantidadDeEmpleados p e = (p,longitud(rolesImplicadosEnElProyecto p (rolesEmpresa e)))






