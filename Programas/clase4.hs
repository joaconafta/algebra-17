--fib calcula el n-esimo numero de fibonacci
fib :: Integer -> Integer
fib n | n == 0 = 1
      | n == 1 = 1
      | otherwise = fib (n-1) + fib(n-2)

--Calcula factorial de n 
fact :: Integer -> Integer
fact n | n == 0 = 1
       | n > 0 = n * (fact (n-1))

--Calcula el n-esimo termino de la sucesion
an :: Integer -> Integer
an n | n == 1 = 2
     | n > 1 = 2 * (n-1) * (an (n-1)) + (2 ^ ((n-1)+1)) * (fact (n-1))

--Calcula el n-esimo termino de la sucesion
bn :: Integer -> Integer
bn n | n == 1 = 1
     | n == 2 = 2
     | n > 2 = (n-2)*(bn (n-1)) + 2*((n-2)+1)* (bn (n-2))

--Calcula el n-esimo termino de la sucesion
cn :: Integer -> Integer
cn n | n == 1 = (-3)
     | n == 2 = 6
     | n > 2 && (not(esPar (n-2))) = (-(cn (n-1)) - 3)
     | n > 2 && (esPar (n-2)) = (cn (n-1)) + 2 * (cn (n-2)) + 9

--Sumatoria de i, desde i=1 hasta i=n (No funciona para negativos)
sumatoria :: Integer -> Integer
sumatoria n | n == 0 = 0
            | otherwise = n + sumatoria (n-1)

--Implementar y dar el tipo de 4 funciones Ej5 Pctica 2
f1 :: Integer -> Integer
f1 n | n == 0 = 2^n
     | n > 0 = (2^n) + f1 (n-1)

f2 :: Integer -> Float -> Float
f2 n q | n == 1 = q ^ n
       | n > 1 = q ^ n + f2 (n-1) q

f3 :: Integer -> Float -> Float
f3 n q = f2 (2*n) q

f4 :: Integer -> Float -> Float
f4 n q =  ((f3 n q) - (f2 (n-1) q)) / 2

--Devuelve si un numero es +, - ó 0
signo n | n > 0 = 1
        | n == 0 = 0
        | n < 0 = -1

--c)Devuelve el valor absoluto de n
valorAbsoluto :: Integer -> Integer
valorAbsoluto n = (signo n) * n

esPar :: Integer -> Bool
esPar n | (valorAbsoluto n) > 1 = esPar (n-2)
      | n == 0 = True
      | n == 1 = False

multiploDe3 :: Integer -> Bool
multiploDe3 n | (valorAbsoluto n) > 2 = multiploDe3 (n - 3)
              | n == 0 = True
              | n == 1 || n == 2 = False

sumaImpares :: Integer -> Integer
sumaImpares n | n == 0 = 0
              | n >= 1 = (2*n - 1) + sumaImpares (n-1)

dobleFact :: Integer -> Integer
dobleFact n | n == 0 = 1
            | n > 0 =  n * dobleFact (n - 2)

infinito :: Integer -> Integer
infinito n | n == 0 = 1
           | otherwise = infinito (n-1)