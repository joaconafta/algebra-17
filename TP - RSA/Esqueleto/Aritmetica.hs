module Aritmetica where
import Catedra
import Data.Tuple
import Data.Bits
--import System.Random --Agregado para funcion propia elegidorPrimos


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
criba :: Integer -> Set Integer
criba n = primosHastaN (n-1)

--(3)
coprimoCon :: Integer -> Integer
coprimoCon n = n-1

--(4)
inversoMultiplicativo :: Integer -> Integer -> Integer
inversoMultiplicativo n m | mcd == 1 = corregirInversoMult s m 
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

--Corrige cuando el inverso multiplicativo es negativo le suma el modulo para dejarlo positivo
corregirInversoMult :: Integer -> Integer -> Integer
corregirInversoMult inv m | inv < 0 = inv+m
                          | otherwise = inv

--Funcion propia para elegir de forma aleatoria dos numeros primos de la criba
--elegidorPrimos :: Set Integer -> (Integer, Integer)
--elegidorPrimos xs = (primoRnd, primoRnd2)
--  where primoRnd = elegirPrimo xs (randomRIO (1, length xs))
--        primoRnd2 = elegirPrimo xs (randomRIO (1, length xs))

--elegirPrimo :: Set Integer -> Integer -> Integer
--elegirPrimo (x:xs)
----------------------------------------------------------------------------------------------