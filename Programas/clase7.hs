--Toma 3 elementos y los convierte en una lista
listar :: a -> a -> a -> [a]
listar x y z = [x,y,z]

--Lista estrictamente decreciente de enteros que comienza con 1 y termina con -100.
ej2 = [1,0..(-100)]


--Devuelve la suma de los elementos de una lista x
sumatoria2 :: [Integer] -> Integer
sumatoria2 [] = 0
sumatoria2 x = head (x) + sumatoria (tail x)

--Idem anterior pero con Pattern matching
sumatoria :: [Integer] -> Integer
sumatoria [] = 0
sumatoria (x: xs) = sumatoria xs + x

--Indica si un elemento n pertenece o no a la lista x
pertenece2 :: Integer -> [Integer] -> Bool
pertenece2 n [] = False
pertenece2 n x = head x == n || pertenece n (tail x)

--Idem anterior pero con Pattern matching
pertenece :: Integer -> [Integer] -> Bool
pertenece n [] = False
pertenece n (x:xs) = x == n || pertenece n xs

tieneDosElementos :: [a] -> Bool
tieneDosElementos (_:_:[]) = True
tieneDosElementos _ = False

--Devuelve la productoria de los elementos
productoria :: [Integer] -> Integer
productoria [] = 1
productoria (x:xs) = x * productoria xs

--Dado un numero n y una lista xs le suma n a cada elemento de xs
sumarN :: Integer -> [Integer] -> [Integer]
sumarN n (x:[]) = [n + x]
sumarN n (x:xs) = (n + x):(sumarN n xs)

--Dada una lista, suma el ultimo elemento a cada elemento de la lista
sumarElUltimo :: [Integer] -> [Integer]
sumarElUltimo x = sumarN (head (reverse x)) x

--Dada una lista, suma el primer elemento a cada elemento de la lista
sumarElPrimero :: [Integer] -> [Integer]
sumarElPrimero x = sumarN (head x) x

--Devuelve si un numero es +, - ó 0
signo n | n > 0 = 1
        | n == 0 = 0
        | n < 0 = -1

--c)Devuelve el valor absoluto de n
valorAbsoluto :: Integer -> Integer
valorAbsoluto n = (signo n) * n

par :: Integer -> Bool
par n | (valorAbsoluto n) > 1 = par (n-2)
      | n == 0 = True
      | n == 1 = False

--Devuelve una lista con los elementos pares de la lista original
pares :: [Integer] -> [Integer]
pares [] = []
pares (x:xs) | par x = x:(pares xs)
             | otherwise = pares xs

--Veo si un k es divisor de n
esDivisor n k = (mod n k) == 0

--Dado un numero n y una lista xs, devuelve una lista con los multiplos de n en xs
multiplosDeN :: Integer -> [Integer] -> [Integer]
multiplosDeN n [] = []
multiplosDeN n (x:xs) | esDivisor x n = x:(multiplosDeN n xs)
                      | otherwise = multiplosDeN n xs

--Dado un numero n y una lista, elimina la 1er aparicion de n en la lista
quitar :: Integer -> [Integer] -> [Integer]
quitar n [] = []
quitar n (x:xs) | x == n = xs
                | otherwise = x : (quitar n xs)

--Indica si una lista tiene elementos repetidos
hayRepetidos :: [Integer] -> Bool
hayRepetidos (x:[]) = False
hayRepetidos (x:xs) | x == head xs = True
                    | otherwise = hayRepetidos (x:(tail xs)) || hayRepetidos xs

--Elimina los elementos repetidos
eliminarRepetidos :: [Integer] -> [Integer]
eliminarRepetidos (x:[]) = x:[]
eliminarRepetidos (x:xs) | x == head xs = eliminarRepetidos (xs)
                         | x /= head xs && hayRepetidos (x:xs) = eliminarRepetidos((head xs):(eliminarRepetidos (x:(tail xs))))
                         | otherwise = (x:xs)
                         
--Calcula el elemento maximo de una lista
maximo :: [Integer] -> Integer
maximo (x:[]) = x
maximo (x:xs) | x > head xs = maximo (x:(tail xs))
              | otherwise = maximo xs

--Ordena los elementos de forma creciente
ordenar :: [Integer] -> [Integer]
ordenar [] = []
ordenar (x:xs) | esOrdenada (x:xs) =  (x:xs)
               | otherwise = ordenar((head xs):(ordenar (x:(tail xs))))

esOrdenada :: [Integer] -> Bool
esOrdenada (x:[]) = True
esOrdenada (x:xs) | x > head xs = False
                  | otherwise = esOrdenada xs
               
