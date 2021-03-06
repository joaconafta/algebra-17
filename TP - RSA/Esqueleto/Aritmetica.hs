module Aritmetica where
import Catedra
import Data.Tuple
import Data.Bits


--(1)
--Algoritmo de Euclides extendido
mcdExt :: Integer -> Integer -> (Integer, (Integer, Integer))
mcdExt a 0 = (a, (1, 0))
mcdExt a b = (m, (s, t))
  where s = t1
        t = (s1 - t1*q)
        q = div a b
        (m, (s1, t1)) = mcdExt b r
        r = mod a b


--(2)
criba :: Integer -> Set Integer
criba n = eratostenes [2..(n-1)]


--(3)
--(n-2) para que 1 < coprimo < n-1
coprimoCon :: Integer -> Integer
coprimoCon n = buscoCoprimoCon n (n-2) 


--(4)
--Calcula inverso multiplicativo de n modulo m (obtiene x tal que x*n = 1 (mod m))
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
--Criba de eratostenes
eratostenes :: Set Integer -> Set Integer
eratostenes [] = []
eratostenes (x:xs) | x^2 > last (x:xs) = (x:xs) -- last de (x:xs) para contemplar el caso en que [x] (en "criba 2" y en "criba 5")
                   | otherwise = x:eratostenes (filter (\n-> mod n x /= 0) xs) 

--Busco un m que sea coprimo con n
buscoCoprimoCon :: Integer -> Integer -> Integer
buscoCoprimoCon n m | mcd == 1 = m
                    | otherwise = buscoCoprimoCon n (m-1)
  where (mcd,(_,_)) = mcdExt n m
