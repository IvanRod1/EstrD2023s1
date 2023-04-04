                                {-Tipos recursivos simples-}
    {-Celdas con bolitas-}

data Color = Azul | Rojo deriving Show
data Celda = Bolita Color Celda | CeldaVacia deriving Show

-- Bolita Rojo (Bolita Azul (Bolita Rojo (Bolita Azul CeldaVacia))) ejemplo
    {-1-}
celdaEjemplo = Bolita Rojo (Bolita Azul (Bolita Rojo (Bolita Azul CeldaVacia)))
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
sacar c (Bolita color celda) = if sonColoresIguales c color
                then sacar c celda
                else Bolita color celda  

--D
ponerN :: Int -> Color -> Celda -> Celda
--Dado un número n, un color c, y una celda, agrega n bolitas de color c a la celda
--ponerN 0 _ _ = CeldaVacia
ponerN cant c celda= if cant > 0
                     then ponerN (cant - 1) c (Bolita c celda)
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
hayTesoro Fin = False
hayTesoro c = esTesoro c || hayTesoro (camino c) 

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
pasosHastaTesoro Fin = 0
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

arbolEjemploPaint2 :: Tree Int
arbolEjemploPaint2 = NodeT 9 
                            (NodeT 10 
                                (NodeT 20 
                                     (NodeT 50 
                                        EmptyT 
                                            (NodeT 1 
                                                (NodeT 5 EmptyT EmptyT) 
                                                    (NodeT 3 EmptyT EmptyT))) EmptyT)
                                                                                    EmptyT) 
                                                                                         EmptyT

arbolSinArboles :: Tree Int
arbolSinArboles = NodeT 2 EmptyT EmptyT

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

leaves (NodeT x a1 a2) = if not (esArbolVacio a1 && esArbolVacio a2)
                         then leaves 
                         else x : leaves 



esHoja :: Tree a -> Bool
-- Una hoja es un arbol cuyos arboles son vacios. Indica si el arbol dado es una hoja
esHoja (NodeT _ EmptyT EmptyT) = True
esHoja _ = False

{-unArbolDeOtroArbol :: Tree a -> Tree a 
--Dado un arbol, si alguno de sus arboles hijos es un arbol vacio, devuelve el arbol que no lo sea. Si ambos arboles son vacios, devuelve arbol vacio

unArbolDeOtroArbol (NodeT _ a1 EmptyT) = a1
unArbolDeOtroArbol (NodeT _ EmptyT a2) = a2
unArbolDeOtroArbol (NodeT _ a1 a2) = NodeT _ a1 a2 -}

--7
heightT :: Tree a -> Int
--Dado un árbol devuelve su altura
heightT (NodeT _ a1 a2) = if profundidadArbol1 a1 > profundidadArbol2 a2
                          then profundidadArbol1 a1
                          else profundidadArbol2 a2

-----------------------------------------------------------
profundidadArbol1 :: Tree a -> Int
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
                                   else 1 + profundidadArbol2 (unArbolDeOtroArbol a2)-}
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
elementoArbol(NodeT x _ _ ) = x


--8
mirrorT :: Tree a -> Tree a 
--Dado un árbol devuelve el árbol resultante de intercambiar el hijo izquierdo con el derecho,
--en cada nodo del árbol
mirrorT EmptyT = EmptyT
mirrorT (NodeT x a1 a2) = NodeT x (mirrorT (invertirArboles a2)) (mirrorT(invertirArboles a1)) -- x no la uso 

invertirArboles :: Tree a -> Tree a
invertirArboles EmptyT = EmptyT
invertirArboles (NodeT x a1 a2) = NodeT x a2 a1 -- x no la uso

--9
toList :: Tree a -> [a]
--Dado un árbol devuelve una lista que representa el resultado de recorrerlo en modo in-order.
--Nota: En el modo in-order primero se procesan los elementos del hijo izquierdo, luego la raiz
--y luego los elementos del hijo derecho
--toList EmptyT = []
toList (NodeT x a1 a2) =  elementosDelArbol a1 ++ [x] ++ elementosDelArbol a2

elementosDelArbol :: Tree a -> [a]
elementosDelArbol EmptyT = []
elementosDelArbol (NodeT x a1 a2) = x : elementosDelArbol a1 ++ elementosDelArbol a2

--10
--levelN :: Int -> Tree a -> [a]
{-Dados un número n y un árbol devuelve una lista con los nodos de nivel n. El nivel de un
nodo es la distancia que hay de la raíz hasta él. La distancia de la raiz a sí misma es 0, y la
distancia de la raiz a uno de sus hijos es 1.
Nota: El primer nivel de un árbol (su raíz) es 0.-}
{-levelN...............
toList n a = if n > 0
             then toList (n - 1) a
             else toList-}

elementoADistanciaNDeRaiz :: Int -> Tree a -> a 
--Devuelve el elemento que este en el nodo que esta a una distancia n de la raiz
elementoADistanciaNDeRaiz _ EmptyT = error "No hay elemento"
elementoADistanciaNDeRaiz n a = if n > 0
                                then elementoADistanciaNDeRaiz (n - 1) (unArbolDeOtroArbol a) 
                                else elementoArbol a







