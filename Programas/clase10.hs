type Polinomio = [Float]

--Evalua p(x) con x = n
evaluar :: Polinomio -> Float -> Float
evaluar [] _ = 0
evaluar (x:xs) n = x + n*(evaluar xs n)

--Calcula la derivada de p(x)
derivada :: Polinomio -> Polinomio
derivada (x:xs) = derivCont (xs) 1

--Funcion auxiliar de Derivada
derivCont :: Polinomio -> Float -> Polinomio
derivCont [] _ = []
derivCont (x:xs) i = (i*x):(derivCont xs (i+1))

--Calcula la derivada N-esima de p(x)
derivadaNesima :: Integer -> Polinomio -> Polinomio
derivadaNesima 0 xs = xs
derivadaNesima n xs = derivadaNesima (n-1) (derivada xs)

--Suma dos polinomios
suma :: Polinomio -> Polinomio -> Polinomio
suma xs ys = quitarCeros (sumarPoli xs ys)

--Funcion auxiliar para sumar polinomios
sumarPoli :: Polinomio -> Polinomio -> Polinomio
sumarPoli xs [] = xs
sumarPoli [] ys = ys
sumarPoli (x:xs) (y:ys) = (x+y):(sumarPoli xs ys)

--Saca todos los 0 a la derecha de la lista
quitarCeros :: Polinomio -> Polinomio
quitarCeros xs | last xs == 0 = quitarCeros (init xs)
               | otherwise = xs

--Multiplica p(x) por n
productoPorEscalar :: Float -> Polinomio -> Polinomio
productoPorEscalar 0 _ = []
productoPorEscalar _ [] = []
productoPorEscalar n (x:xs) = (n*x):(productoPorEscalar n xs)

--Multiplica a p(x) por un ax^i
productoPorMonomio :: Float -> Integer -> Polinomio -> Polinomio
productoPorMonomio a n xs = aumentarGrado n (productoPorEscalar a xs)

--Aumenta el grado de p(x)
aumentarGrado :: Integer -> Polinomio -> Polinomio
aumentarGrado 0 xs = xs
aumentarGrado n xs = 0:(aumentarGrado (n-1) xs)

--Calcula el producto entre dos polinomios
producto :: Polinomio -> Polinomio -> Polinomio
producto [] _ = []
producto _ [] = []
producto xs ys = productoCont xs 0 ys

productoCont :: Polinomio -> Integer -> Polinomio -> Polinomio
productoCont [] i ys = []
productoCont (x:xs) i ys = suma (productoPorMonomio x i ys) (productoCont xs (i+1) ys)
