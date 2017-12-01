module Crack where
import Catedra
import Aritmetica
import RSA


--(9)
romper :: Clpub -> Clpri
romper (e, n) = (d, n)
  where (p, q) = factorizar n
        d = inversoMultiplicativo e phi
        phi = (phiEuler p q)


--(8)
espia :: Clpub -> Cifrado -> Mensaje
espia (e, n) c = decodificador (d, n) c
  where (d, _) = romper (e, n)



--FUNCIONES AUXILIARES------------------------------------------------------------------------
--Factoriza un numero producto de dos primos
factorizar :: Integer -> (Integer, Integer)
factorizar n = (p, q)
  where p = primerFactor n (criba cotaN)
        q = div n p
        cotaN = floor (sqrt (fromInteger n)) + 1

--Encuentra el primer primo que divide a n
primerFactor :: Integer -> Set Integer -> Integer
primerFactor n (x:xs) | mod n x == 0 = x
                      | otherwise = primerFactor n xs


                    

