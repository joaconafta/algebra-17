module Aritmetica where
import Catedra
import Data.Tuple
import Data.Bits


--(1)
mcdExt :: Integer -> Integer -> (Integer, (Integer, Integer))
mcdExt a 0 = (a, (1, 0))
mcdExt a b = (m, (s, t))
  where s = t1
        t = (s1 - t1*q)
        q = div a b
        (m, (s1, t1)) = mcdExt b r
        r = mod a b

--(2)
--(n-1) para que n no aparezca dentro de la criba
criba :: Integer -> Set Integer
criba n = primosHastaN (n-1) 

--(3)
--(n-2) para que 1 < coprimo < n-1
coprimoCon :: Integer -> Integer
coprimoCon n = buscoCoprimoCon n (n-2) 

--(4)
inversoMultiplicativo :: Integer -> Integer -> Integer
inversoMultiplicativo n m | mcd == 1 = mod s m 
                          | otherwise = error "No existe inverso multiplicativo"
  where (mcd, (s,t)) = mcdExt n m

-- Función de regalo para exponenciar "rápido"
modExp :: Integer -> Integer -> Integer -> Integer
modExp b 0 m = 1
modExp b e m = t * modExp ((b * b) `mod` m) (shiftR e 1) m `mod` m
  where t = if testBit e 0 then b `mod` m else 1


--FUNCIONES AUXILIARES------------------------------------------------------------------------
--Veo si un k es divisor de n
esDivisor :: Integer -> Integer -> Bool
esDivisor n k = mod n k == 0

--Calcula el menor divisor de n desde k
menorDivisorDesde :: Integer -> Integer -> Integer
menorDivisorDesde n k | esDivisor n k = k
                      | otherwise = menorDivisorDesde n (k+1)

--Devuelve si n es primo o no
esPrimo :: Integer -> Bool
esPrimo n = (menorDivisorDesde n 2 == n)

--Devuelve una lista de todos los primos menores o iguales que n
primosHastaN :: Integer -> Set Integer
primosHastaN 1 = []
primosHastaN n | esPrimo n = p ++ [n]
               | otherwise = p
  where p = primosHastaN (n-1)

--Busco un m que sea coprimo con n
buscoCoprimoCon :: Integer -> Integer -> Integer
buscoCoprimoCon n m | mcd == 1 = m
                    | otherwise = buscoCoprimoCon n (m-1)
  where (mcd,(_,_)) = mcdExt n m
----------------------------------------------------------------------------------------------