--yLogico
yLogico :: Bool -> Bool -> Bool
yLogico True True = True
yLogico _ _ = False

--oLogico
oLogico :: Bool -> Bool -> Bool
oLogico False False = False
oLogico _ _ = True

--implica
implica :: Bool -> Bool -> Bool
implica True False = False
implica _ _ = True


--Sumatoria de i, desde i=1 hasta i=n (No funciona para negativos)
sumatoria :: Integer -> Integer
sumatoria n | n == 0 = 0
            | otherwise = n + sumatoria (n-1)

--sumaGaussiana toma un entero no negativo y devuelve la suma de todos los enteros positivos menores o iguales que el
sumaGaussiana :: Integer -> Integer
sumaGaussiana n = sumatoria n

--algunoEsCero devuelve True sii alguna de las componentes de la tupla es 0
algunoEsCero :: (Integer, Integer, Integer) -> Bool
algunoEsCero (0,_,_) = True
algunoEsCero (_,0,_) = True
algunoEsCero (_,_,0) = True
algunoEsCero (_,_,_) = False

--Idem (otra forma)
algunoEsCero2 :: (Integer, Integer, Integer) -> Bool
algunoEsCero2 (x,y,z) = x*y*z == 0

--productoInterno dados dos vectores v1 = (x1, y1), v2 = (x2, y2) ∈ R2, calcula su producto interno ⟨v1, v2⟩ = x1x2 + y1y2.
productoInterno :: (Float, Float) -> (Float, Float) -> Float
productoInterno (x1,y1) (x2,y2) = x1*x2 + y1*y2

--Devuelve si un numero es +, - ó 0
signo n | n > 0 = 1
        | n == 0 = 0
        | n < 0 = -1

--c)Devuelve el valor absoluto de n
valorAbsoluto :: Integer -> Integer
valorAbsoluto n = (signo n) * n

par :: Integer -> Bool
par n | (valorAbsoluto n) > 1 = par (n-2)
      | n == 0 = True
      | n == 1 = False

--sumaDigitos determina la suma de digitos de un numero positivo.
sumaDigitos :: Integer -> Integer
sumaDigitos n | n < 10 = n
              | otherwise = (mod n 10) + sumaDigitos (div n 10)

--igualDigitos determina si todos los digitos de un numero son iguales.
igualDigitos :: Integer -> Bool
igualDigitos n | n < 10 = True
               | (mod n 10) == (mod (div n 10) 10) = igualDigitos (div n 10)
               | otherwise = False

--collatz devuelve la cantidad de terminos de la secuencia de collatz para n
collatz :: Integer -> Integer
collatz n | n == 1 = 1
          | par n = 1 + collatz (div n 2)
          | otherwise = 1 + collatz (3*n + 1)

--DUDA - HAY FORMA DE HACERLO SIN PASAR DOS PARAMETROS?
--maxCollatz devuelve el n que mas terminos tiene en la secuencia
maxCollatz :: Integer -> Integer -> Integer
maxCollatz n m | n == 1 = m
               | collatz m < collatz n = maxCollatz (n-1) n
               | otherwise = maxCollatz (n-1) m

