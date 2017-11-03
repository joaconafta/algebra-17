type Polinomio = [Float]

evaluar :: Polinomio -> Float -> Float
evaluar [] _ = 0
evaluar (x:xs) n = x + n*(evaluar xs n)

derivada :: Polinomio -> Polinomio
derivada (x:xs) = derivCont (xs) 1

derivCont :: Polinomio -> Float -> Polinomio
derivCont [] _ = []
derivCont (x:xs) i = (i*x):(derivCont xs (i+1))

derivadaNesima :: Integer -> Polinomio -> Polinomio
derivadaNesima 0 xs = xs
derivadaNesima n xs = derivadaNesima (n-1) (derivada xs)

suma :: Polinomio -> Polinomio -> Polinomio
suma xs ys = quitarCeros (sumarPoli xs ys)

sumarPoli :: Polinomio -> Polinomio -> Polinomio
sumarPoli xs [] = xs
sumarPoli [] ys = ys
sumarPoli (x:xs) (y:ys) = (x+y):(sumarPoli xs ys)

quitarCeros :: Polinomio -> Polinomio
quitarCeros xs | last xs == 0 = quitarCeros (init xs)
               | otherwise = xs

productoPorEscalar :: Float -> Polinomio -> Polinomio
productoPorEscalar 0 _ = []
productoPorEscalar _ [] = []
productoPorEscalar n (x:xs) = (n*x):(productoPorEscalar n xs)

productoPorMonomio :: Float -> Integer -> Polinomio -> Polinomio
productoPorMonomio a n xs = aumentarGrado n (productoPorEscalar a xs)

aumentarGrado :: Integer -> Polinomio -> Polinomio
aumentarGrado 0 xs = xs
aumentarGrado n xs = 0:(aumentarGrado (n-1) xs)

producto :: Polinomio -> Polinomio -> Polinomio
producto (x:xs) ys = productoCont x 0 ys

productoCont (:x:xs) i ys = productoPorMonomio x i ys
