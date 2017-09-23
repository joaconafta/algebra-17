--Devuelve si un numero es +, - ó 0
signo n | n > 0 = 1
        | n == 0 = 0
        | n < 0 = -1

--a)Devuelve el valor absoluto de n
valorAbsoluto n | n >= 0 = n
                | n < 0 = -n

--b)Devuelve el valor absoluto de n
valorAbsoluto2 n | n >= 0 = n
                 | otherwise = -n

--c)Devuelve el valor absoluto de n
valorAbsoluto3 n = (signo n) * n

--Calcula el maximo entre 2 numeros
maximo n1 n2 | n1 >= n2 = n1
             | n1 < n2 = n2

--Calcula el maximo entre 3 numeros
maximo3 n1 n2 n3 =  maximo (maximo n1 n2) n3

--Devuelve si el entero ingresado es par o no
esPar :: Integer -> Bool
esPar n = (mod n 2) == 0

--Devuelve si el 1er entero es multiplo del otro o no
esMultiploDe :: Integer -> Integer -> Bool
esMultiploDe n1 n2 = (mod n2 n1) == 0

--Dados dos numeros crea un par ordenado
crearPar :: a -> b -> (a, b)
crearPar a b = (a, b)

--Dado un par ordenado, invierte las coordenadas
invertir :: (a, b) -> (b, a)
invertir t = (snd t, fst t)

--Dado un vector en R2 devuelve la norma
normaVectorial :: (Float, Float) -> Float
normaVectorial p = sqrt((fst p) ^ 2 + (snd p) ^ 2)

--Dados dos puntos en R2 devuelve la resta
resta :: Num a => (a, a) -> (a, a) -> (a, a)
resta p1 p2 = (fst p1 - fst p2, snd p1 - snd p2)

--Dados dos puntos en R2 calcula la distancia entre ambos puntos
distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos p1 p2 = normaVectorial (resta p1 p2)

--Practica 1
--Ejercicio 31
--IV)
f1 :: Float -> (Float, Float)
f1 x = (exp x, 1 - exp x)

--V)
f2 :: Integer -> Integer
f2 n | esPar n = div n 2
     | otherwise = n + 1

--Ejercicio 32
--I)
cuadrado n = n * n
triple x = 3 * x

f :: Integer -> Integer
f n | esMultiploDe 6 n = div (cuadrado n) 2
     | otherwise = triple n + 1

g :: (Integer, Integer) -> Integer
g nm = fst nm * (snd nm + 1)

h :: (Integer, Integer) -> Integer
h nm = f (g nm)