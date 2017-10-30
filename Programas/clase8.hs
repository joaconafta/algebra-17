type Set a = [a]
--Sinonimo para distinguir listas de conjuntos

--Conjunto vacio
vacio :: Set Integer
vacio = []

agregar :: Integer -> Set Integer -> Set Integer
agregar n xs | elem n xs = xs
             | otherwise = n:xs

--Devuelve si un conjunto esta incluido en otro
incluido :: Set Integer -> Set Integer -> Bool
incluido [] ys = True
incluido (x:xs) ys | (elem x ys) = incluido xs ys
                   | otherwise = False

--Devuelve si dos conjuntos son iguales
iguales :: Set Integer -> Set Integer -> Bool
iguales xs ys = incluido xs ys && incluido ys xs

agregarATodas :: Integer -> Set (Set Integer) -> Set (Set Integer)
agregarATodas n [] = []
agregarATodas n (xs:xss) = (agregar n xs) :(agregarATodas n xss)

--Genera todos los subconjuntos de un conjunto
partesDeConjunto :: Set Integer -> Set (Set Integer)
partesDeConjunto [] = [[]]
partesDeConjunto (x:xs) = (agregarATodas x (partesDeConjunto xs)) ++ (partesDeConjunto xs)

--Genera todos los subconjuntos del conjunto {1,2,3,...,n}
partes :: Integer -> Set (Set Integer)
partes 0 = [[]]
partes n = agregarATodas n (partes (n - 1)) ++ partes (n - 1)

--Dados dos conjuntos genera todos los pares posibles 
productoCartesiano :: Set Integer -> Set Integer -> Set (Integer, Integer)
productoCartesiano _ [] = []
productoCartesiano [] _ = []
productoCartesiano (x:xs) (y:ys) = (x, y):(productoCartesiano [x] ys) ++ (productoCartesiano xs (y:ys))

--Dado un conjunto c y una longitud l genera todas las posibles listas de longitud l a partir de elementos de c.
variaciones :: Set Integer -> Integer -> [[Integer]] 
 
 
--Ejemplo> variaciones [4, 7] 3
--[[4, 4, 4], [4, 4, 7], [4, 7, 4], [4, 7, 7], [7, 4, 4], [7, 4, 7], [7,
--7, 4], [7, 7, 7]]

