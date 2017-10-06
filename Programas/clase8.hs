type Set a = [a]
--Sinonimo para distinguir listas de conjuntos

--Devuelve si un conjunto esta incluido en otro
incluido :: Set Integer -> Set Integer -> Bool
incluido [] ys = True
incluido (x:xs) ys | (elem x ys) = incluido xs ys
                   | otherwise = False

--Devuelve si dos conjuntos son iguales
iguales :: Set Integer -> Set Integer -> Bool
iguales xs ys = incluido xs ys && incluido ys xs

--partesDeConjunto :: Set Integer -> Set (Set Integer)
--partesDeConjunto xs =
