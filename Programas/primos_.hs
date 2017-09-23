--DUDA - EJERCICIO PRIMOS forma 1-------------------------------------------------------------------
--Veo si un k es divisor de n
esDivisor n k = (mod n k) == 0

--Calcula el menor divisor de n desde k
menorDivisorDesde :: Integer -> Integer -> Integer
menorDivisorDesde n k | esDivisor n k = k
                      | otherwise = menorDivisorDesde n (k+1)

--Devuelve si n es primo o no
esPrimo :: Integer -> Bool
esPrimo n = (menorDivisorDesde n 2 == n)

nesimoPrimo :: Integer -> Integer -> Integer -> Integer
nesimoPrimo n k m | m == n = (k-1)
                  | esPrimo k =  nesimoPrimo n (k+1) (m+1)
                  | otherwise =  nesimoPrimo n (k+1) m

--FUNCIONA PERO NO ES LA MEJOR MANERA (ESTOY PASANDO UN PARAMETRO DE MAS)
--esSumaDeDosPrimos dado n determina si se puede escribir como suma de dos numeros primos
esSumaDeDosPrimos2 :: Integer -> Integer -> Bool
esSumaDeDosPrimos2 n k | k == n = False
                       | esPrimo (n - (nesimoPrimo k 2 0)) = True
                       | otherwise = esSumaDeDosPrimos2 n (k+1)

--DUDA - EJERCICIO PRIMOS forma 2-------------------------------------------------------------------
--esSumaDeDosPrimos dado n determina si se puede escribir como suma de dos numeros primos
esSumaDeDosPrimos :: Integer -> Bool
esSumaDeDosPrimos n | par n = True
                    | otherwise = esPrimo (n-2)

--goldbach conjetura, todo numero par mayor que 2 se puede escribir como suma de dos primos. Probar la conjetura
goldbach :: Integer -> Bool
goldbach n | n == 2 = True
           | par n = goldbach (n-1)
---------------------------------------------------------------------------------------------------