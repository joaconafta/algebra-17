--Devuelve si un numero es +, - ó 0
signo n | n > 0 = 1
        | n == 0 = 0
        | n < 0 = -1

--c)Devuelve el valor absoluto de n
valorAbsoluto3 n = (signo n) * n

--unidades dado un entero devuelve el digito de las unidades
unidades :: Integer -> Integer
unidades n = mod (valorAbsoluto3 n) 10

suma x y = x + y

resto n = (mod n 2)

--sumaUnidades3 dados 3 enteros devuelve la suma de los digitos de las unidades de los 3
sumaUnidades3 :: Integer -> Integer -> Integer -> Integer
sumaUnidades3 n1 n2 n3 = suma (suma (unidades n1) (unidades n2)) (unidades n3)

--sumaUnidades32 :: Integer -> Integer -> Integer -> Integer
--sumaUnidades32 n1 n2 n3 = suma (suma (uni n1) (uni n2)) (uni n3) where uni = unidades

--Devuelve si el entero ingresado es par o no
esPar :: Integer -> Bool
esPar n = (mod n 2) == 0

--cantidadDeImpares dados 3 numeros calcula cuantos impares hay
cantidadDeImpares :: Integer -> Integer -> Integer -> Integer
cantidadDeImpares n1 n2 n3 = suma (suma (resto n1) (resto n2)) (resto n3)

--todosImpares dados 3 numeros enteros determina si son todos impares
todosImpares :: Integer -> Integer -> Integer -> Bool
todosImpares n1 n2 n3 = not(esPar n1 || esPar n2 || esPar n3)

todosImpares2 :: Integer -> Integer -> Integer -> Bool
todosImpares2 n1 n2 n3 = cantidadDeImpares n1 n2 n3 == 3

--alMenosUnImpar dados 3 numeros enteros determina si almenos uno de ellos es impar
alMenosUnImpar :: Integer -> Integer -> Integer -> Bool
alMenosUnImpar n1 n2 n3 = not(esPar n1 && esPar n2 && esPar n3)

alMenosUnImpar2 :: Integer -> Integer -> Integer -> Bool
alMenosUnImpar2 n1 n2 n3 = cantidadDeImpares n1 n2 n3 >= 1

--alMenosDosImpares dados 3 numeros enteros determina si al menos dos de ellos son impares
alMenosDosImpares :: Integer -> Integer -> Integer -> Bool
alMenosDosImpares n1 n2 n3 = cantidadDeImpares n1 n2 n3 >= 2

--alMenosDosPares dados 3 numeros enteros determina si al mnenos dos de ellos son pares
alMenosDosPares :: Integer -> Integer -> Integer -> Bool
alMenosDosPares n1 n2 n3 = cantidadDeImpares n1 n2 n3 <= 1

--RELACIONES
--7) dados dos enteros a,b determinar si aRb
--a∼b si tienen la misma paridad
r1 :: Integer -> Integer -> Bool
r1 a b = esPar a && esPar b

--a∼b si 2a + 3b es divisible por 5
r2 :: Integer -> Integer -> Bool
r2 a b = (mod (suma (2*a) (3*b)) 5) == 0

--a∼b si los digitos de las unidades de a,b y ab son todos distintos
r3 :: Integer -> Integer -> Bool
r3 a b = (unidades a /= unidades b) && (unidades b /= unidades (a*b)) && (unidades a /= unidades (a*b))

--8)
--Dada relacion de equivalencia asociada a la particion R=(-inf,3)U[3,+inf)
--determinar tipo y, dados dos numeros x,y e R determinar si x∼y
r4 :: Float -> Float -> Bool
r4 x y = (x < 3 && y < 3) || (x >= 3 && y >= 3)

--9)
--Idem 8, R=(-inf,3)U[3,7)U[7,+inf)
r5 :: Float -> Float -> Bool
r5 x y = (x < 3 && y < 3) || (x >= 3 && x < 7 && y >= 3 && y < 7) || (x >= 7 && y >= 7)

--10)
--Dados (a,b) y (p,q) en ZxZ - {(0,0)} determinar el tipo
--implementar funciones que si (a,b)R(p,q)
--a) (a,b)∼(p,q) si existe k e Z tal que (a,b) = k(p,q)
--r6 :: (Integer,Integer) -> (Integer,Integer) -> Bool
--r6 (a,b) (p,q) =

--11)
--sumaC calcula la suma de dos numeros complejos a + bi se expresa como (a,b) (Float,Float)
sumaC :: (Float,Float) -> (Float,Float) -> (Float,Float)
sumaC (a,b) (c,d) = (a+c, b+d)

--12)
--productoC calcula el producto de dos numeros complejos
productoC :: (Float,Float) -> (Float,Float) -> (Float,Float)
productoC (a,b) (c,d) = (a*c + b*d*(-1), a*d + b*c)

--productoPorRealC calcula el producto de un numero real con uno complejo
productoPorRealC :: Float -> (Float,Float) -> (Float,Float)
productoPorRealC r (a,b) = (r*a,r*b)

--conjugadoC calcula el conjugado de un numero complejo
conjugadoC :: (Float,Float) -> (Float,Float)
conjugadoC (a,b) = (a,(-1)*b)

--inversoC calcula el inverso de un numero complejo
inversoC :: (Float,Float) -> (Float,Float)
inversoC (a,b) = productoPorRealC (1 / (a**2 + b**2)) (conjugadoC (a,b))

--13)
--raices dados a,b,c en R devuelve las raices de ax^2 + bx + c 
--raices :: Float -> Float -> Float -> (Float,Float) -> (Float,Float)
--raices a b c | b**2 - 4*a*c >= 0 = (((-1)*b + sqrt(b**2 - 4*a*c))/(2*a),0) (((-1)*b - sqrt(b**2 - 4*a*c))/(2*a),0)