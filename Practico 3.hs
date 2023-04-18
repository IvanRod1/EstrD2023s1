                                {-Tipos recursivos simples-}
    {-Celdas con bolitas-}

data Color = Azul | Rojo deriving Show
data Celda = Bolita Color Celda | CeldaVacia deriving Show

-- Bolita Rojo (Bolita Azul (Bolita Rojo (Bolita Azul CeldaVacia))) ejemplo
    {-1-}
celdaEjemplo = Bolita Rojo (Bolita Rojo (Bolita Azul (Bolita Rojo (Bolita Azul CeldaVacia))))
celdaVacia = CeldaVacia

--A
nroBolitas :: Color -> Celda -> Int
--Dados un color y una celda, indica la cantidad de bolitas de ese color. Nota: pensar si ya
--existe una operación sobre listas que ayude a resolver el problema.
nroBolitas _ CeldaVacia = 0
nroBolitas cab (Bolita color celda) = unoSiEsElColorDado cab color + nroBolitas cab celda -- cab significa color a buscar

unoSiEsElColorDado :: Color -> Color ->Int
-- Denota 1 si los colores dados son iguales, 0 si no 
unoSiEsElColorDado cab c = if sonColoresIguales cab c then 1 else 0  -- cab significa color a buscar

sonColoresIguales:: Color -> Color -> Bool
-- Indica si los dos colores dados son iguales
sonColoresIguales Azul Azul = True
sonColoresIguales Rojo Rojo = True
sonColoresIguales _ _ = False

--B
poner :: Color -> Celda -> Celda
--Dado un color y una celda, agrega una bolita de dicho color a la celda.   
poner c celda =  Bolita c celda  

--C
sacar :: Color -> Celda -> Celda
--Dado un color y una celda, quita una bolita de dicho color de la celda. Nota: a diferencia de
--Gobstones, esta función es total.

sacar c CeldaVacia = celdaVacia
sacar c (Bolita color celda) = {-if sonColoresIguales c color
                               then sacar c celda
                               else Bolita color celda-}
                               if not (sonColoresIguales c color)
                               then  Bolita color (sacar c celda)
                               else celda


--D
ponerN :: Int -> Color -> Celda -> Celda
--Dado un número n, un color c, y una celda, agrega n bolitas de color c a la celda
--ponerN 0 _ _ = CeldaVacia
ponerN cant c celda= {-if cant > 0
                     then ponerN (cant - 1) c (Bolita c celda)
                     else celda-}

                     if cant > 0
                     then ponerN (cant - 1) c (poner c celda)
                     else celda

                        {- Camino hacia el tesoro-}

data Objeto = Cacharro | Tesoro deriving Show
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino deriving Show

caminoEjemplo = Nada (Nada (Cofre [Tesoro] Fin)) 
caminoConTesoroAlPrinicipio = Cofre [Tesoro] Fin
caminoConCacharro = Nada (Nada (Cofre [Cacharro] Fin)) 
caminoConVariosTesoros = Nada (Cofre [Tesoro] (Nada (Cofre [Tesoro] (Nada (Cofre [Tesoro] Fin)))))

--A
hayTesoro :: Camino -> Bool
--Indica si hay un cofre con un tesoro en el camino.
{-hayTesoro (Cofre _ _) = True
hayTesoro _ = False-}

{-hayTesoro Fin = False
hayTesoro c = esTesoro c || hayTesoro (camino c)-}

hayTesoro (Cofre [Tesoro] _) = True
hayTesoro (Nada camino) = hayTesoro camino
hayTesoro _ = False


esTesoro :: Camino -> Bool
--Indica si hay un teseoro en el camino dado
esTesoro (Cofre [Tesoro] _) = True
esTesoro _ = False

camino :: Camino -> Camino
--Dado un camino, devuelve el camino
camino (Nada camino) = camino
camino (Cofre _ camino) = camino
camino _ = Fin
-- Nada (Nada (Cofre [Tesoro] Fin)) 

--B
pasosHastaTesoro :: Camino -> Int
--Indica la cantidad de pasos que hay que recorrer hasta llegar al primer cofre con un tesoro.
--Si un cofre con un tesoro está al principio del camino, la cantidad de pasos a recorrer es 0.
--Precondición: tiene que haber al menos un tesoro.
--pasosHastaTesoro Fin = 0
pasosHastaTesoro c = if not (esTesoro c)
                     then 1 + pasosHastaTesoro (camino c)
                     else pasosHastaTesoro (camino c)

--C
hayTesoroEn :: Int -> Camino -> Bool
--Indica si hay un tesoro en una cierta cantidad exacta de pasos. Por ejemplo, si el número de
--pasos es 5, indica si hay un tesoro en 5 pasos.
hayTesoroEn n c = if n > 0
                     then hayTesoroEn (n - 1) (camino c)
                     else esTesoro c

--D
alMenosNTesoros :: Int -> Camino -> Bool
--Indica si hay al menos “n” tesoros en el camino
alMenosNTesoros n c = cantidadTesorosEnElCamino c >= n 

cantidadTesorosEnElCamino :: Camino -> Int
--Describe la cantidad de tesoros que hay en el camino dado
cantidadTesorosEnElCamino Fin = 0
cantidadTesorosEnElCamino c = if esTesoro c
                              then 1 + cantidadTesorosEnElCamino (camino c)
                              else cantidadTesorosEnElCamino (camino c)

--E
cantTesorosEntre :: Int -> Int -> Camino -> Int
--Dado un rango de pasos, indica la cantidad de tesoros que hay en ese rango. Por ejemplo, si
--el rango es 3 y 5, indica la cantidad de tesoros que hay entre hacer 3 pasos y hacer 5. Están
--incluidos tanto 3 como 5 en el resultado.
cantTesorosEntre pMin pMax c = cantidadDeTesorosAlHacerNPasos pMax c - cantidadDeTesorosAlHacerNPasos pMin c -- mal

--avanzar pasos

avanzarN :: Int -> Camino -> Camino
--Dado un numero, avanza la cantidad de veces que diga el mismo devolviendo un camino con los pasos avanzados
avanzarN 0 c = c
avanzarN n c = if n > 0
               then avanzarN (n - 1) (camino c)
               else c


cantidadDeTesorosAlHacerNPasos :: Int -> Camino -> Int 
--Describe la cantidad de tesoros que hay haciendo una cantidad de pasos dados en el camino dado
cantidadDeTesorosAlHacerNPasos 0 _ = 0
cantidadDeTesorosAlHacerNPasos p c = if p > 0
                                     then unoSiHayTesoroCeroSino c + cantidadDeTesorosAlHacerNPasos (p - 1) (camino c)
                                     else cantidadDeTesorosAlHacerNPasos p (camino c)

unoSiHayTesoroCeroSino :: Camino -> Int
--Denota 1 si hay tesoro en el camino, cero en el caso contrario
unoSiHayTesoroCeroSino c = if esTesoro c then 1 else 0


                            {-Árboles binarios-}

data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show

arbolNumeros :: Tree Int
arbolNumeros = NodeT 4 (NodeT 8 (NodeT 2 EmptyT EmptyT ) EmptyT) EmptyT

arbolEjemplo :: Tree Int
arbolEjemplo = NodeT 9 (NodeT 10 (NodeT 20 (NodeT 50 EmptyT EmptyT) EmptyT) EmptyT) EmptyT

arbolBasico :: Tree [Char]
arbolBasico = NodeT "Hola" (NodeT "Como" EmptyT EmptyT) (NodeT "Estas" EmptyT EmptyT)

arbolConfuso :: Tree [Char]
arbolConfuso = NodeT "Hola" (NodeT "Como" (NodeT "Buenas" EmptyT EmptyT) (NodeT "Bono" EmptyT (NodeT "Sol" EmptyT EmptyT))) (NodeT "Estas" EmptyT (NodeT "Luna" (NodeT "Estrella" EmptyT EmptyT ) (NodeT "Galaxia" EmptyT EmptyT )))

arbolEjemploPaint :: Tree Int
arbolEjemploPaint = NodeT 9 (NodeT 10 (NodeT 20 (NodeT 50 EmptyT (NodeT 1 (NodeT 5 EmptyT EmptyT) (NodeT 3 EmptyT EmptyT))) EmptyT) EmptyT) EmptyT

arbolSinArboles :: Tree Int
arbolSinArboles = NodeT 2 EmptyT EmptyT

arbolFidel :: Tree Int
arbolFidel = NodeT 1 (NodeT 2 (NodeT 4 (NodeT 8 EmptyT EmptyT)
                          EmptyT)
                 (NodeT 5 EmptyT
                          EmptyT))
        (NodeT 3 (NodeT 6 EmptyT
                          (NodeT 9 EmptyT EmptyT))
                 (NodeT 7 EmptyT
                          EmptyT))

--1
sumarT :: Tree Int -> Int
--Dado un árbol binario de enteros devuelve la suma entre sus elementos.
sumarT EmptyT = 0
sumarT (NodeT x a1 a2) = x + sumarT a1 + sumarT a2

--2
sizeT :: Tree a -> Int
--Dado un árbol binario devuelve su cantidad de elementos, es decir, el tamaño del árbol (size
--en inglés).
sizeT EmptyT = 0
sizeT (NodeT _ a1 a2) = 1 + sizeT a1 + sizeT a2

--3
mapDobleT :: Tree Int -> Tree Int
--Dado un árbol de enteros devuelve un árbol con el doble de cada número.
mapDobleT EmptyT = EmptyT
mapDobleT (NodeT x a1 a2) = NodeT (x * 2) (mapDobleT a1) (mapDobleT a2)

--4
perteneceT :: Eq a => a -> Tree a -> Bool
--Dados un elemento y un árbol binario devuelve True si existe un elemento igual a ese en el
--árbol
perteneceT a EmptyT = False
perteneceT a (NodeT x a1 a2) = a == x || perteneceT a a1 || perteneceT a a2

--5
aparicionesT :: Eq a => a -> Tree a -> Int
--Dados un elemento e y un árbol binario devuelve la cantidad de elementos del árbol que son
--iguales a e.
aparicionesT _ EmptyT = 0
aparicionesT a (NodeT x a1 a2) = unoSiCeroSino (a == x) + aparicionesT a a1 + aparicionesT a a2

unoSiCeroSino :: Bool -> Int
unoSiCeroSino True = 1
unoSiCeroSino _ = 0

--6
leaves :: Tree a -> [a]
-- Dado un árbol devuelve los elementos que se encuentran en sus hojas.
-- esta funcion debe retornar la lista de los elementos que tienen las hojas
leaves EmptyT = []

--leaves (NodeT x a1 a2) = x : leaves a1 ++ leaves a2
{-leaves a = if esHoja a
           then elementoArbol a : leaves (unArbolDeOtroArbol a)
           else leaves (unArbolDeOtroArbol a) -}

leaves (NodeT a a1 a2) = if esArbolVacio a1 && esArbolVacio a2
                         then a : leaves a1 ++ leaves a2
                         else leaves a1 ++ leaves a2



esHoja :: Tree a -> Bool
-- Una hoja es un arbol cuyos arboles son vacios. Indica si el arbol dado es una hoja
--esHoja (NodeT _ EmptyT EmptyT) = True
--esHoja _ = False
esHoja _ = False
esHoja(NodeT _ a1 a2) = esArbolVacio a1 && esArbolVacio a2

{-unArbolDeOtroArbol :: Tree a -> Tree a 
--Dado un arbol, si alguno de sus arboles hijos es un arbol vacio, devuelve el arbol que no lo sea. Si ambos arboles son vacios, devuelve arbol vacio

unArbolDeOtroArbol (NodeT _ a1 EmptyT) = a1
unArbolDeOtroArbol (NodeT _ EmptyT a2) = a2
unArbolDeOtroArbol (NodeT _ a1 a2) = NodeT _ a1 a2 -}

--7
heightT :: Tree a -> Int
--Dado un árbol devuelve su altura maxima
heightT EmptyT = 0
heightT (NodeT x a1 a2) =  unoSiCeroSino(not (esHoja a1) && not (esHoja a2)) + dameElMayor (heightT a1)  (heightT a2)
                    ---unoSiCeroSino(not (esHoja a1) && not (esHoja a2))

dameElMayor :: Int -> Int -> Int
--Dados dos numeros, devuelve el mayor de estos
dameElMayor x y = if x > y then x else y


{-heightT :: Tree a -> Int
--Dado un árbol devuelve su altura maxima
heightT (NodeT _ a1 a2) = if profundidadArbol a1 > profundidadArbol a2
                          then profundidadArbol a1
                          else profundidadArbol a2-}

-----------------------------------------------------------
profundidadArbol :: Tree a -> Int
profundidadArbol EmptyT = 0
profundidadArbol (NodeT _ a1 a2) = --unoSiCeroSino(not (esArbolVacio a1) || not (esArbolVacio a2)) + profundidadArbol a1 + profundidadArbol a2
                                   --unoSiCeroSino(not (esArbolVacio a1) || esHoja a2 || not (esArbolVacio a2) || esHoja a1) + profundidadArbol a1 + profundidadArbol a2
                                     unoSiCeroSino(esHoja a1 || esHoja a2 || not (esArbolVacio a2) || not (esArbolVacio a1)) + profundidadArbol a1 + profundidadArbol a2 

{-profundidadArbol1 :: Tree a -> Int
profundidadArbol1 EmptyT = 0
--profundidadArbol1 (NodeT _ a1 _) =
profundidadArbol1 a = if not (esArbolVacio a)
                      then 1 + profundidadArbol1 (unArbolDeOtroArbol (primerArbol a)) + profundidadArbol2 (unArbolDeOtroArbol (primerArbol a))
                      else profundidadArbol1 (unArbolDeOtroArbol (primerArbol a)) + profundidadArbol2 (unArbolDeOtroArbol (primerArbol a))
    
    
    --1 + profundidadArbol1 (unArbolDeOtroArbol a1) + profundidadArbol2 (unArbolDeOtroArbol a1)

                                   {-if esArbolVacio a1
                                   then profundidadArbol1 (unArbolDeOtroArbol a1) + profundidadArbol2 (unArbolDeOtroArbol a1)
                                   else 1 + profundidadArbol1 (unArbolDeOtroArbol a1)-}

profundidadArbol2 :: Tree a -> Int
profundidadArbol2 EmptyT = 0
--profundidadArbol2 (NodeT _ _ a2) = 
profundidadArbol2 a = if not (esArbolVacio a)
                      then 1 + profundidadArbol1 (unArbolDeOtroArbol (segundoArbol a)) + profundidadArbol2 (unArbolDeOtroArbol (segundoArbol a))
                      else profundidadArbol1 (unArbolDeOtroArbol (primerArbol a)) + profundidadArbol2 (unArbolDeOtroArbol (primerArbol a)) 
    
    --1 + profundidadArbol1 (unArbolDeOtroArbol a2) + profundidadArbol2 (unArbolDeOtroArbol a2)
    
    
                                   {-if esArbolVacio a2
                                   then profundidadArbol1 (unArbolDeOtroArbol a2) + profundidadArbol2 (unArbolDeOtroArbol a2)
                                   else 1 + profundidadArbol2 (unArbolDeOtroArbol a2)-}-}
-------------------------------------------------------------                         
esArbolVacio :: Tree a -> Bool
--Indica si el arbol dado es vacio
esArbolVacio EmptyT = True
esArbolVacio _ = False

unArbolDeOtroArbol :: Tree a -> Tree a 
--Dado un arbol, si alguno de sus arboles hijos es un arbol vacio, devuelve el arbol que no lo sea. Si ambos arboles son vacios, devuelve arbol vacio

unArbolDeOtroArbol (NodeT _ a1 EmptyT) = a1
unArbolDeOtroArbol (NodeT _ EmptyT a2) = a2 


primerArbol:: Tree a -> Tree a 
primerArbol (NodeT _ a1 _) = a1 

segundoArbol :: Tree a -> Tree a
segundoArbol (NodeT _ _ a2) = a2 

elementoArbol :: Tree a -> a  
elementoArbol(NodeT a _ _ ) = a


--8

mirrorT :: Tree a -> Tree a 
--Dado un árbol devuelve el árbol resultante de intercambiar el hijo izquierdo con el derecho,
--en cada nodo del árbol

mirrorT EmptyT = EmptyT
mirrorT (NodeT x t1 t2) = invertirArboles (NodeT x (mirrorT t1) (mirrorT t2))

invertirArboles :: Tree a -> Tree a
--dado un arbol, devuelve otro arbol con sus arboles cambiados de posicion
invertirArboles EmptyT = EmptyT
invertirArboles (NodeT x t1 t2) = (NodeT x t2 t1)

{-mirrorT :: Tree a -> Tree a 
--Dado un árbol devuelve el árbol resultante de intercambiar el hijo izquierdo con el derecho,
--en cada nodo del árbol
mirrorT EmptyT = EmptyT
mirrorT (NodeT a a1 a2) = NodeT a (mirrorT (invertirArboles a2)) (mirrorT(invertirArboles a1)) -- x no la uso -}

{-invertirArboles :: Tree a -> Tree a
invertirArboles EmptyT = EmptyT
invertirArboles (NodeT a a1 a2) = NodeT a a2 a1 -- x no la uso-}


--9
toList :: Tree a -> [a]
--Dado un árbol devuelve una lista que representa el resultado de recorrerlo en modo in-order.
--Nota: En el modo in-order primero se procesan los elementos del hijo izquierdo, luego la raiz
--y luego los elementos del hijo derecho
toList EmptyT = []
toList (NodeT x t1 t2) =  toList t1 ++ [x] ++ toList t2



{-toList2 :: Tree a -> [a]
--Dado un árbol devuelve una lista que representa el resultado de recorrerlo en modo in-order.
--Nota: En el modo in-order primero se procesan los elementos del hijo izquierdo, luego la raiz
--y luego los elementos del hijo derecho
--toList2 EmptyT = []
toList2 (NodeT a a1 a2) =  elementosDelArbol a1 ++ [a] ++ elementosDelArbol a2

elementosDelArbol :: Tree a -> [a]
elementosDelArbol EmptyT = []
elementosDelArbol (NodeT a a1 a2) = a : elementosDelArbol a1 ++ elementosDelArbol a2-}

--10
levelN :: Int -> Tree a -> [a]
{-Dados un número n y un árbol devuelve una lista con los nodos de nivel n. El nivel de un
nodo es la distancia que hay de la raíz hasta él. La distancia de la raiz a sí misma es 0, y la
distancia de la raiz a uno de sus hijos es 1.
Nota: El primer nivel de un árbol (su raíz) es 0.-}
levelN _ EmptyT = []
levelN 0 (NodeT a _ _) = [a]
levelN n (NodeT a a1 a2) = if n > 0
                           then levelN (n - 1) a1 ++ levelN (n - 1) a2 
                           else a : levelN n a1 ++ levelN n a2 
 

{-elementosADistanciaNDeRaiz :: Int -> Tree a -> [a] 
--Devuelve el elemento que este en el nodo que esta a una distancia n de la raiz
elementosADistanciaNDeRaiz _ EmptyT = []
elementosADistanciaNDeRaiz  0 (NodeT x _ _) = [x]
elementosADistanciaNDeRaiz  n (NodeT x a1 a2) = if n > 0
                                              then  elementosADistanciaNDeRaiz  (n - 1) a1 ++ elementosADistanciaNDeRaiz  (n - 1) a2 
                                              else x : elementosADistanciaNDeRaiz  n a1 ++ elementosADistanciaNDeRaiz  n a2 -}


--11

listPerLevel :: Tree a -> [[a]]
--Dado un árbol devuelve una lista de listas en la que cada elemento representa un nivel de
--dicho árbol.
listPerLevel EmptyT = []
listPerLevel (NodeT a a1 a2) =  [a] : unirListasDeListas (listPerLevel a1) (listPerLevel a2)

unirListasDeListas :: [[a]] -> [[a]] -> [[a]]
--Dados dos listas de listas, retorna una lista de listas con todos los elementos de la primer y segunda lista juntos
unirListasDeListas [] ys = ys
unirListasDeListas xs [] = xs
unirListasDeListas (x:xs) (y:ys) = (x ++ y) : unirListasDeListas xs ys


--12
ramaMasLarga :: Tree a -> [a]
--Devuelve los elementos de la rama más larga del árbol
ramaMasLarga EmptyT = []
ramaMasLarga (NodeT x t1 t2) =  x : elejirListaMax (ramaMasLarga t1) (ramaMasLarga t2)

elejirListaMax :: [a] -> [a] -> [a]
--Dado dos listas, devuelve la lista con mas elementos
elejirListaMax ls1 ls2=  if longitud ls1 > longitud ls2
                          then ls1
                          else ls2

longitud :: [a] -> Int  --practico 2
longitud [] = 0
longitud (_:xs) = 1 + longitud xs

{-ramaMasLarga :: Tree a -> [a]
--Devuelve los elementos de la rama más larga del árbol
ramaMasLarga EmptyT = []
ramaMasLarga (NodeT _ t1 t2) = if profundidadArbol t1 > profundidadArbol t2
                               then elementosDeUnaRama t1 -- .... ramaMasLarga t1 .... ramaMasLarga t2
                               else elementosDeUnaRama t2
 

elementosDeUnaRama :: Tree a -> [a]
elementosDeUnaRama EmptyT = []
elementosDeUnaRama (NodeT a t1 t2) = a : elementosDeUnaRama t1 ++ elementosDeUnaRama t2-}

--12
todosLosCaminos :: Tree a -> [[a]]
--Dado un árbol devuelve todos los caminos, es decir, los caminos desde la raiz hasta las hojas.
todosLosCaminos EmptyT = []
todosLosCaminos (NodeT x t1 t2) = [x] : agregarElementoAListas x (todosLosCaminos t1) ++ agregarElementoAListas x (todosLosCaminos t2)
                                    --juntarListasDeListas (agregarElementoAListas x (todosLosCaminos t1)) (agregarElementoAListas x (todosLosCaminos t2))
                                    -- prueba (agregarElementoAListas x (todosLosCaminos t2)) (agregarElementoAListas x (todosLosCaminos t1))
                                    --unirListasDeListas2  (agregarElementoAListas x (todosLosCaminos t1)) (agregarElementoAListas x (todosLosCaminos t2))
                                    --agregarElementoAListas x (todosLosCaminos t1) --agregarElementoAListas x (todosLosCaminos t2)
agregarElementoAListas :: a -> [[a]] -> [[a]]
agregarElementoAListas a []= []
agregarElementoAListas x (y:ys) =  (x : y) : agregarElementoAListas x ys


{-nirListasDeListas2 :: [[a]] -> [[a]] -> [[a]]
--Dados dos listas de listas, retorna una lista de listas con todos los elementos de la primer y segunda lista juntos
unirListasDeListas2 [] ys = ys
unirListasDeListas2 xs [] = xs
unirListasDeListas2 (x:xs) (y:ys) = [x] ++ [y] ++ unirListasDeListas xs ys -}

juntarListasDeListas :: [[a]] -> [[a]] -> [[a]]
juntarListasDeListas [] ys = []
juntarListasDeListas xs [] = []
juntarListasDeListas (x:xs) (y:ys) = [x] ++ [y] ++ juntarListasDeListas xs ys
                            

{-todosLosCaminos :: Tree a -> [[a]]
--Dado un árbol devuelve todos los caminos, es decir, los caminos desde la raiz hasta las hojas.
todosLosCaminos EmptyT = []
todosLosCaminos (NodeT x t1 t2) = agregarRaiz x (todosLosCaminos t1) (todosLosCaminos t2)
                                            --unirListasDeListas (arbolALista2 (todosLosCaminos t1)) (arbolALista2 (todosLosCaminos t2))
                                            --todosLosCaminos (arbolALista2 t1) ++ todosLosCaminos (arbolALista t2)
-- me deberia dar [[1,2,4,8],[1,2,5],[1,2],[1,3,6,9],[1,3,7],[1,3],[],[1]] arbol fidel

agregarRaiz :: a -> [[a]] -> [[a]] -> [[a]]
agregarRaiz a [[]] [[]] = [[]]
agregarRaiz a (x:xs) (y:ys) =  [a : x] ++ [a : y] ++ agregarRaiz a xs ys 


arbolALista :: Tree a -> [[a]]
arbolALista EmptyT = []
arbolALista (NodeT x t1 t2) = [x] ++ arbolALista t1-}


-- [ [],[1],[1,2],[1,2,4],[1,2,4,8],[1,2,5],[1,3],[1,3,6],[1,3,7] ] ejemplo discord de fidel

{-arbolALista :: Tree a -> [Tree a]
arbolALista EmptyT = []
arbolALista (NodeT a t1 t2) = if not (esArbolVacio t1)
                               then t1 : arbolALista t2
                               else arbolALista t1 ++ arbolALista t2-}

arbolALista2 :: Tree a -> [a]
arbolALista2 EmptyT = []
arbolALista2 (NodeT a t1 t2) = if not (esHoja t1) || not (esHoja t2) 
                               then a : arbolALista2 t1 ++ arbolALista2 t2
                               else  arbolALista2 t1 ++  arbolALista2 t2

{-caminoARaices :: Tree a -> [[a]]
caminoARaices EmptyT = []
caminoARaices (NodeT a t1 t2) = [[a]] ++ caminoARaices t1 ++ caminoARaices t2-}


{-subtarea :: Tree a -> [[a]]
subtarea EmptyT = []
subtarea (NodeT a t1 t2) = [a] : todosLosCaminos' t1 ++ [a] : todosLosCaminos' t2-}


                            {-Expresiones Aritméticas-}
data ExpA = Valor Int | Sum ExpA ExpA | Prod ExpA ExpA | Neg ExpA deriving Show

ejemploAritmetica :: ExpA 
ejemploAritmetica = Prod (Prod (Valor 2) (Valor 3)) (Prod (Valor 2) (Valor 3))

ejemploAritmetica2 :: ExpA
ejemploAritmetica2 =  Prod (Valor 2) (Valor 3)
    --Prod (Prod (Valor 2) (Valor 3)) (Prod (Valor 2) (Valor 3))

ejemploAritmetica3:: ExpA
ejemploAritmetica3 = Neg (Valor (negate(-2)))
 
--1
eval :: ExpA -> Int
--Dada una expresión aritmética devuelve el resultado evaluarla.
eval (Sum x y) = eval x + eval y
eval (Prod x y) = eval x * eval y
eval (Neg x) = eval x * (-1)
eval (Valor x) = x 



{-eval :: ExpA -> Int
--Dada una expresión aritmética devuelve el resultado evaluarla.
eval (Prod x y) = valorEnteroDe(accederAlValorProd x) * valorEnteroDe(accederAlValorProd y)  --valor(Valor(accederAlValor x)) * valor(Valor (accederAlValor y)) / valor(Prod(accederAlValor x) (accederAlValor y))
eval (Sum x y) = valorEnteroDe(accederAlValorSum x) + valorEnteroDe(accederAlValorSum y)
eval (Neg x) = valorEnteroDe(accederAlValorNeg x)
eval _ = error "No se puede operar con un solo numero"-}
--eval (Sum (Valor x) (Valor y)) = x + y
--eval (Neg (Valor x)) = (-1) * x
--eval (Valor x) = x

{-accederAlValorProd:: ExpA -> ExpA
accederAlValorProd(Prod x y) = if esUnNumero x && esUnNumero y
                               then Valor (valorEnteroDe x * valorEnteroDe y)
                               else accederAlValorProd (Prod (accederAlValorProd x)(accederAlValorProd y))
                                   
                                   --accederAlValorProd (Prod (accederAlValor x) (accederAlValor y))
                                   --accederAlValorProd((accederAlValor x)(accederAlValor y))
accederAlValorSum :: ExpA -> ExpA
accederAlValorSum (Sum (Valor 0) y) = y
accederAlValorSum (Sum x (Valor 0)) = x
accederAlValorSum (Sum x y) =  if esUnNumero x && esUnNumero y
                               then Valor (valorEnteroDe x + valorEnteroDe y)
                               else accederAlValorSum(Sum (accederAlValorSum x)(accederAlValorSum y))



accederAlValorNeg :: ExpA -> ExpA 
accederAlValorNeg (Neg x) = if esUnNumero x 
                            then Valor((-1) * valorEnteroDe x)
                            else accederAlValorNeg (Neg (accederAlValorNeg x)) 

{-accederAlValor :: ExpA -> ExpA
accederAlValor (Prod x y) = Prod (accederAlValorProd x)(accederAlValorProd x)-} -- <- necesito mejorar esta funcion


esUnNumero :: ExpA -> Bool
--Dado un dato de tipo ExpA, indica si este es un valor
{-esUnNumero (Prod _ _) = False
esUnNumero (Sum _ _) = False
esUnNumero (Neg _) = False-}
esUnNumero (Valor _) = True
esUnNumero _ = False

valorEnteroDe :: ExpA -> Int
--Funcion observadora de Valor
valorEnteroDe (Valor x) = x

esProducto :: ExpA -> Bool
--Dado un dato de tipo ExpA, indica si este es un producto
esProducto (Prod _ _) = True
esProducto _ = False-}


--2
simplificar :: ExpA -> ExpA
--Dada una expresión aritmética, la simplifica según los siguientes criterios (descritos utilizando
--notación matemática convencional):
 {- a) 0 + x = x + 0 = x
    b) 0 * x = x * 0 = 0
    c) 1 * x = x * 1 = x
    d) - (- x) = x  -} 
simplificar (Sum x y)  = Valor (simplificarSiSuma (eval x) (eval y))
simplificar (Prod x y) = Valor (simplificarSiProd (eval x) (eval y))
simplificar _ = error "No se puede simplificar"

simplificarSiSuma :: Int -> Int -> Int
simplificarSiSuma x 0 = x
simplificarSiSuma 0 y = y
simplificarSiSuma x y = x + y

simplificarSiProd :: Int -> Int -> Int
simplificarSiProd x 0 = 0
simplificarSiProd 0 y = 0
---------------------------
simplificarSiProd x 1 = x
simplificarSiProd 1 y = y
---------------------------
simplificarSiProd x (-1) = if x < 0 then x else (-x)
simplificarSiProd x y = x * y

{-simplificarSi :: Int -> Int -> ExpA
simplificarSi x y = Valor (if x == 0
                           then y
                           else if y == 0
                           then x
                           else x + y)

simplificarSi (Prod x y) =  Valor (if x == 1 then y
                                   else if y == 1 then x
                                   else if x == 0 then 0
                                   else if x > 0  && y == x then x * (-1)
                                   else if y > 0  && x == y then y * (-1)                     
                                   else x * y)-}
                                








{-simplificar :: ExpA -> ExpA
--Dada una expresión aritmética, la simplifica según los siguientes criterios (descritos utilizando
--notación matemática convencional):
 {- a) 0 + x = x + 0 = x
    b) 0 * x = x * 0 = 0
    c) 1 * x = x * 1 = x
    d) - (- x) = x  -}

simplificar (Sum (Valor 0) y) = y
simplificar (Sum x (Valor 0)) = x
simplificar (Prod (Valor 1) y) = y
simplificar (Prod x (Valor 1)) = x
--simplificar (Neg (Valor (x < 0))) = valorEnteroDe x * (-1)
simplificar(Neg (Valor x)) = if x < 0
                             then Valor (x * (-1))
                             else Valor x
simplificar _ = error "No se puede simplificar"-}

