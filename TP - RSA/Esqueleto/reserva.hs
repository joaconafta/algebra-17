--DUDAS
--COPRIMOCON PUEDO ELEGIR n-1 O LO TENGOQ UE HACER CON LAS FUNCIONES DE ABAJO?

--FUNCIONES AUXILIARES PARA COPRIMOCON (3) ---------------------
--Calcula el maximo comun divisor entre a y b con el algoritmo de Euclides
mcd :: Integer -> Integer -> Integer
mcd a 0 = a
mcd a b = mcd b r
  where r = mod a b

--Devuelve si dos numeros son coprimos
sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos a b = mcd a b == 1

coprimoMenorQueN :: Integer -> Integer -> Integer
coprimoMenorQueN n i | sonCoprimos n i = i
                     | otherwise = coprimoMenorQueN n (i-1)
----------------------------------------------------------------