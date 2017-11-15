module RSA where
import Catedra
import Aritmetica


--(5)
claves :: Integer -> Integer -> (Integer, Integer, Integer)
claves p q = (e, d, n)
  where n = p * q
        e = coprimoCon phi
        d = inversoMultiplicativo e phi 
        phi = (phiEuler p q)

--(6)
codificador :: Clpub -> Mensaje -> Cifrado
codificador (e,n) m = codificadorInts (e,n) (aEnteros m)

--(7)
decodificador :: Clpri -> Cifrado -> Mensaje
decodificador (d,n) c = aChars(decodificadorInts (d,n) c)


--FUNCIONES AUXILIARES------------------------------------------------------------------------
--Cacula phi(n) = (p-1)*(q-1)
phiEuler :: Integer -> Integer -> Integer
phiEuler p q = (p-1)*(q-1)

--Codifica el mensaje cifrado en [Integer]
codificadorInts :: Clpub -> Set Integer -> Cifrado
codificadorInts (e,n) [] = []
codificadorInts (e,n) (m:ms) = (codificarDigito (e,n) m):(codificadorInts (e,n) ms)

--Codifica un digito del mensaje en Integer
codificarDigito :: Clpub -> Integer -> Integer
codificarDigito (e,n) m | mcd == 1 = modExp m e n
                        | otherwise = (-m)
  where (mcd,_) = mcdExt m n 

--Decodifica el mensaje cifrado en [Integer]
decodificadorInts :: Clpri -> Cifrado -> Set Integer
decodificadorInts (d,n) [] = []
decodificadorInts (d,n) (c:cs) = (decodificarDigito (d,n) c):(decodificadorInts (d,n) cs)

--Decodifica un digito del mensaje en Integer
decodificarDigito :: Clpri -> Integer -> Integer
decodificarDigito (d,n) c | c >= 0 = modExp c d n
                          | otherwise = (-c)
----------------------------------------------------------------------------------------------