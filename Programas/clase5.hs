--Calcula factorial de n 
fact :: Integer -> Integer
fact n | n == 0 = 1
       | n > 0 = n * (fact (n-1))

--eAprox aproxima el numero e
eAprox :: Integer -> Float
eAprox n | n == 0 = 1
         | otherwise = eAprox (n-1) +  (1 / (fromInteger(fact n)))

--e devuelve el termino 100 de la sucesion anterior
e :: Float
e = eAprox 100

--Devuelve si un numero es +, - ó 0
signo n | n > 0 = 1
        | n == 0 = 0
        | n < 0 = -1

--c)Devuelve el valor absoluto de n
valorAbsoluto :: Integer -> Integer
valorAbsoluto n = (signo n) * n

--parteEntera calcula la parte entera de un numero real
parteEntera :: Float -> Integer
parteEntera n | (n >= 0 && n < 1) = 0
              | (n >= 1) = 1 + parteEntera (n-1)
              | (n <= 0) = (-1) + parteEntera (n+1)

parteEntera2 :: Float -> Integer
parteEntera2 n | (n >= 0 && n < 1) = 0
               | otherwise = (signo n * 1) + parteEntera (n - signo n * 1)

--Algoritmo de division
division :: Integer -> Integer -> (Integer, Integer)
division a d | valorAbsoluto a < d = (0,valorAbsoluto a)
             | otherwise = (signo a * 1 + fst(qr'), snd(qr'))
             where qr' = division (a - (signo a)*d) d

--Suma todos los divisores de n
sumaDivisores :: Integer -> Integer
sumaDivisores n = sumarDivisoresDesde n 1

--Veo si un k es divisor de n
esDivisor n k = (mod n k) == 0

--Suma todos los divisores de n desde k
sumarDivisoresDesde :: Integer -> Integer -> Integer
sumarDivisoresDesde n k | k == n = k
                        | esDivisor n k = k + (sumarDivisoresDesde n (k+1))
                        | otherwise = sumarDivisoresDesde n (k+1)

--Calcula el menor divisor de n desde k
menorDivisorDesde :: Integer -> Integer -> Integer
menorDivisorDesde n k | esDivisor n k = k
                      | otherwise = menorDivisorDesde n (k+1)

--Calcula el menor divisor (>1) de n
menorDivisor :: Integer -> Integer
menorDivisor n = menorDivisorDesde n 2

--Devuelve si n es primo o no
esPrimo :: Integer -> Bool
esPrimo n = (menorDivisorDesde n 2 == n)

--Sumatoria dentro de sumatoria f
sumatoriaB :: Integer -> Integer -> Integer
sumatoriaB n m | m == 0 = 0
               | m >= 1 = n ^ m + (sumatoriaB n (m-1))

--Sumatoria Doble
f :: Integer -> Integer -> Integer
f n m | n == 0 = 0
      | n >= 1 = sumatoriaB n m + f (n-1) m 

--sumaPotencias n m que sume todas las potencias de la forma q^a con 1 ≤ q ≤ n y 1 ≤ a ≤ m.
sumaPotencias :: Integer -> Integer -> Integer
sumaPotencias n m = f n m

--sumaRacionales n m que sume todos los numeros racionales de la forma p/q con 1 ≤ p ≤ n y 1 ≤ q ≤ m.
sumatoriaC  :: Integer -> Integer -> Float
sumatoriaC n m | m == 0 = 0
               | m >= 1 = (sumatoriaC n (m-1)) + fromInteger(n) / fromInteger(m)

sumaRacionales :: Integer -> Integer -> Float
sumaRacionales n m | n == 0 = 0
                   | n >= 1 = sumatoriaC n m + sumaRacionales (n-1) m