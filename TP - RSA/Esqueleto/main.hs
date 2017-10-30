import RSA
import Crack
import Aritmetica
import Catedra
import Data.Char

main = do
  test prueba


prueba = ["Learn you a Haskell for Great Good!", "RSA apesta", "Hoogle", "Mentira", "Seguro se rompe con mensajes vacios", "", "jaja"]

test s = do
  mapM_ putStrLn $ magiaPura s


magiaPura [] = []
magiaPura (x:xs) = res : magiaPura xs
  where
    res = "Codificando " ++ show x ++ "\nEl cifrado es " ++ show cif
            ++ "\nBob decodifico " ++ show msg ++ "\nY Miranda espio "
            ++ show esp ++ "\n" -- ++ dbg -- si le dejamos esto manda informacion de debuggeo
    dbg = "[DBG] Mis claves son " ++ show (e,d,n) ++ "\n[DBG] Mis primos " ++ show (p,q) ++ "\n"
    (p, q) = elegidor  $ criba 5000
    (e, d, n) = claves p q
    cif = codificador (d,n) x
    msg = decodificador (e,n) cif
    esp = espia (d,n) cif


elegidor :: Set Integer -> (Integer, Integer)
elegidor (x:y:[]) =  (x, y)
elegidor (x:y:z:[]) = (x, z)
elegidor xs = (xs !! (j+1), xs !! j)
  where n = length(xs)
        j = div n 2
