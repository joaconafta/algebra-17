quitar :: Integer -> [Integer] -> [Integer]
quitar n [] = []
quitar n (x:xs) | n == x = xs
                | otherwise = x:(quitar n xs)


ordenar :: [Integer] -> [Integer]
ordenar [] = []
ordenar l = (maximum l):(ordenar (quitar (maximum l) l))

resta :: [Integer] -> Integer
resta (x:xs) = x - head xs

enProgresion :: Integer -> Integer -> Integer -> Bool
enProgresion a b c = resta (ordenar [a,b,c]) == resta (tail (ordenar [a,b,c]))



--2
esMultiplo :: Integer -> Integer -> Bool
esMultiplo m n = mod m n == 0

potenciasDos:: Integer -> Integer -> Integer
potenciasDos m i | esMultiplo m (2^i) = 1 + potenciasDos m (i+1)
                 | otherwise = 0

valuacion2Adica :: Integer -> Integer
valuacion2Adica m = (potenciasDos m 0) -1

--3
sucesion :: Integer -> Integer
sucesion 1 = 3
sucesion m = 2*sucesion (m-1) + 3

terminos :: Integer -> Integer -> Integer
terminos i n | (sucesion i) < n = 1 + (terminos (i+1) n)
             | otherwise = 0

cuantosTerminos :: Integer -> Integer
cuantosTerminos n = terminos 1 n

--4
sumaDeDos :: [Integer] -> Bool
sumaDeDos (x:xs) = x + head xs == head (tail xs)

esTipoFibo :: [Integer] -> Bool	
esTipoFibo l | length l == 3 = sumaDeDos l
             | length l > 3 && sumaDeDos l = esTipoFibo (tail l)
             | otherwise = False

--5
desplazar :: Integer -> [a] -> [a]
desplazar 0 l = l
desplazar n [] = []
desplazar n (l:ls) = desplazar (n-1) (ls++[l])