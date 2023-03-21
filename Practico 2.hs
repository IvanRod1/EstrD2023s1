                        {- Recursión sobre listas -}
--1
sumatoria :: [Int] -> Int
sumatoria [] = 0 
sumatoria (x:xs) = x + sumatoria xs

--2
longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

--3
sucesores :: [Int] -> [Int]
sucesores [] = []
sucesores (x:xs) = x + 1 : sucesores xs

--4
conjuncion :: [Bool] -> Bool
conjuncion [] = False
conjuncion (b:bs) = b && if null bs
                            then b
                            else last bs

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
                               then [l] ++ (lasDeLongitudMayorA b ls)
                               else lasDeLongitudMayorA b ls

--11

agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal (x:xs) a = if x == []
                          then 
                          else agregarAlFinal xs








