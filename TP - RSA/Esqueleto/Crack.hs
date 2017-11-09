module Crack where
import Catedra
import Aritmetica
import RSA


--(9)
romper :: Clpub -> Clpri
romper (_, n) = (d, n)
  where (_, d, _) = claves p q
        (p, q) = obtenerPrimos n 

--(8)
espia :: Clpub -> Cifrado -> Mensaje
espia (e, n) c = decodificador (d, n) c
  where (d, _) = romper (e, n)


--FUNCIONES AUXILIARES------------------------------------------------------------------------
obtenerPrimos :: Integer -> (Integer, Integer)
obtenerPrimos n = (p, q)
  where [(p, _),(q, _)] = factorizarEnteros n

factorizarEnteros :: Integer -> Set (Integer, Integer)
factorizarEnteros n = factorizar n (criba m)
  where m = mayorFactorPrimo (fromInteger n)

--El mayor factor primo que divide a n es menor o igual que 2*sqrt(n)
mayorFactorPrimo :: Float -> Integer
mayorFactorPrimo n = floor (2 * (sqrt n))

factorizar :: Integer -> Set Integer -> Set (Integer, Integer)
factorizar n [] = []
factorizar n (x:xs) | j == 0 = factorizar m xs
                    | otherwise = (x, j):(factorizar m xs)
  where (m, j) = dividirJVeces n x 0

--Devuelve (n dividido m j veces, j)
dividirJVeces :: Integer -> Integer -> Integer -> (Integer, Integer)
dividirJVeces n m j | esDivisor n m = dividirJVeces (div n m) m (j+1)
                    | otherwise = (n, j)
----------------------------------------------------------------------------------------------