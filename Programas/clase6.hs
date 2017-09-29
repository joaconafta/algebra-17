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

--Veo si un k es divisor de n
esDivisor n k = (mod n k) == 0

--Calcula el menor divisor de n desde k
menorDivisorDesde :: Integer -> Integer -> Integer
menorDivisorDesde n k | esDivisor n k = k
                      | otherwise = menorDivisorDesde n (k+1)

--Devuelve si n es primo o no
esPrimo :: Integer -> Bool
esPrimo n = (menorDivisorDesde n 2 == n)

--DUDA Devuelve el n-esimo numero primo
nesimoPrimo :: Integer -> Integer -> Integer -> Integer
nesimoPrimo n k m | m == n = (k-1)
                  | esPrimo k =  nesimoPrimo n (k+1) (m+1)
                  | otherwise =  nesimoPrimo n (k+1) m

--FUNCIONA PERO NO ES LA MEJOR MANERA (ESTOY PASANDO UN PARAMETRO DE MAS)
--DUDA esSumaDeDosPrimos dado n determina si se puede escribir como suma de dos numeros primos
esSumaDeDosPrimos2 :: Integer -> Integer -> Bool
esSumaDeDosPrimos2 n k | k == n = False
                       | esPrimo (n - (nesimoPrimo k 2 0)) = True
                       | otherwise = esSumaDeDosPrimos2 n (k+1)

--DUDA esSumaDeDosPrimos dado n determina si se puede escribir como suma de dos numeros primos
esSumaDeDosPrimos :: Integer -> Bool
esSumaDeDosPrimos n | par n = True
                    | otherwise = esPrimo (n-2)

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

--DUDA goldbachHasta conjetura, todo numero par mayor que 2 se puede escribir como suma de dos primos. Probar la conjetura hasta n
goldbachHasta :: Integer -> Bool
goldbachHasta n | n == 2 = True
                | n < 2 = False
                | not (par n) = goldbachHasta (n-1)
                | par n && esSumaDeDosPrimos n = goldbachHasta (n-2)
                | otherwise = False

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

--DUDA - HAY FORMA DE HACERLO SIN PASAR DOS PARAMETROS? REHACER
--maxCollatz devuelve el n que mas terminos tiene en la secuencia
maxCollatz :: Integer -> Integer -> Integer
maxCollatz n m | n == 1 = m
               | collatz m < collatz n = maxCollatz (n-1) n
               | otherwise = maxCollatz (n-1) m
