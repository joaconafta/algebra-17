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
--Obtiene los primos p y q de la factorizacion de n
obtenerPrimos :: Integer -> (Integer, Integer)
obtenerPrimos n = (p, q)
  where [(p, _),(q, _)] = factorizarEnteros n

--(m+1) para que aparezca m dentro de la criba por si el numero a factorizar es m^2
factorizarEnteros :: Integer -> Set (Integer, Integer)
factorizarEnteros n = factorizar n ((criba (m+2)))
  where m = mayorFactorPrimo (fromInteger n)

--Todos los factores primos menos el mayor de ellos, son menores a sqrt(n)
mayorFactorPrimo :: Float -> Integer
mayorFactorPrimo n = floor ((sqrt n))

--Funcion auxiliar para factorizar enteros
factorizar :: Integer -> Set Integer -> Set (Integer, Integer)
factorizar 1 _ = []
factorizar n ls | xs == [] = [(n,1)]
                | j == 0 = factorizar m xs
                | otherwise = (x, j):(factorizar m xs)
  where (m, j) = dividirJVeces n x 0
        (x:xs) = ls --Para salvar el caso de que ls sea vacio (solo cuando n es 2, ls es []) 
                    --o cuando ls tiene un solo elemento (solo cuando n es 3, ls es [2])

--Devuelve (n dividido m j veces, j)
dividirJVeces :: Integer -> Integer -> Integer -> (Integer, Integer)
dividirJVeces n m j | esDivisor n m = dividirJVeces (div n m) m (j+1)
                    | otherwise = (n, j)
----------------------------------------------------------------------------------------------