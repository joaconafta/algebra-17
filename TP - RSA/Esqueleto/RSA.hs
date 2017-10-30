module RSA where
import Catedra
import Aritmetica


--(3)
claves :: Integer -> Integer -> (Integer, Integer, Integer)
claves _ _ = (e, d, n)
  where n = 0
        e = 0
        d = 0

--(6)
codificador :: Clpub -> Mensaje -> Cifrado
codificador _ _ = []

--(7)
decodificador :: Clpri -> Cifrado -> Mensaje
decodificador _ _ = ""
