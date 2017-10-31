module RSA where
import Catedra
import Aritmetica


--(5)
claves :: Integer -> Integer -> (Integer, Integer, Integer)
claves p q = (e, d, n)
  where n = p * q
        e = coprimoCon euler
        d = inversoMultiplicativo e euler 
        euler = (funcionEuler p q)

--(6)
codificador :: Clpub -> Mensaje -> Cifrado
codificador (e,n) m = codificadorInts (e,n) (aEnteros m)

--(7)
decodificador :: Clpri -> Cifrado -> Mensaje
decodificador (d,n) c = aChars(decodificadorInts (d,n) c)




---------------------------------------------------------------

funcionEuler :: Integer -> Integer -> Integer
funcionEuler p q = (p-1)*(q-1)

--Codifica un digito del mensaje (un Integer)
codificarDigito :: Clpub -> Integer -> Integer
codificarDigito (e,n) m | mcd == 1 = modExp m e n
                        | otherwise = (-m)
  where (mcd,(s,t)) = mcdExt m n 

--Codifica el mensaje cifrado en [Integer]
codificadorInts :: Clpub -> Cifrado -> Cifrado
codificadorInts (e,n) [] = []
codificadorInts (e,n) (m:ms) = (codificarDigito (e,n) m):(codificadorInts (e,n) ms)

decodificadorInts :: Clpri -> Cifrado -> Cifrado
decodificadorInts (d,n) [] = []
decodificadorInts (d,n) (c:cs) = (decodificarDigito (d,n) c):(decodificadorInts (d,n) cs)

decodificarDigito :: Clpri -> Integer -> Integer
decodificarDigito (d,n) c | c >= 0 = modExp c d n
                          | otherwise = (-c)

