
type Polinomio a = [a]

grado :: (Num a, Eq a) => Polinomio a -> Integer
grado p = deg (quitarCeros p)

deg :: (Num a, Eq a) => Polinomio a -> Integer
deg [p] = 0
deg (p:ps) = 1 + (deg ps)

--Evalua p(x) con x = n
evaluar :: Num a => Polinomio a -> a -> a
evaluar [] _ = 0
evaluar (x:xs) n = x + n*(evaluar xs n)

--Calcula la derivada de p(x)
derivada :: Num a => Polinomio a -> Polinomio a
derivada (x:xs) = derivCont (xs) 1
 where
    derivCont [] _ = []
    derivCont (x:xs) i = (i*x):(derivCont xs (i+1))

--Calcula la derivada N-esima de p(x)
derivadaNesima :: Num a => Integer -> Polinomio a-> Polinomio a
derivadaNesima 0 xs = xs
derivadaNesima n xs = derivadaNesima (n-1) (derivada xs)

--Suma dos polinomios
suma :: (Num a, Eq a) => Polinomio a -> Polinomio a -> Polinomio a
suma xs ys = quitarCeros (sumarPoli xs ys)

--Funcion auxiliar para sumar polinomios
sumarPoli :: (Num a, Eq a) => Polinomio a -> Polinomio a -> Polinomio a
sumarPoli xs [] = xs
sumarPoli [] ys = ys
sumarPoli (x:xs) (y:ys) = (x+y):(sumarPoli xs ys)

--Saca todos los 0 a la derecha de la lista
quitarCeros :: (Num a, Eq a) => Polinomio a -> Polinomio a
quitarCeros xs | last xs == 0 = quitarCeros (init xs)
               | otherwise = xs

productoPorEscalar :: (Num a, Eq a) => a -> Polinomio a -> Polinomio a
productoPorEscalar n p = quitarCeros(aux n p)
  where aux _ [] = []
        aux n (x:xs) = n*x : aux n xs

--Multiplica a p(x) por un ax^i
productoPorMonomio :: (Num a, Eq a) => a -> Integer -> Polinomio a -> Polinomio a
productoPorMonomio a n xs = aumentarGrado n (productoPorEscalar a xs)

--Aumenta el grado de p(x)
aumentarGrado :: Num a => Integer -> Polinomio a -> Polinomio a
aumentarGrado 0 xs = xs
aumentarGrado n xs = 0:(aumentarGrado (n-1) xs)

--Calcula el producto entre dos polinomios
producto :: (Num a, Eq a) => Polinomio a -> Polinomio a -> Polinomio a
producto [] _ = []
producto _ [] = []
producto xs ys = productoCont xs 0 ys
  where productoCont [] i ys = []
        productoCont (x:xs) i ys = suma (productoPorMonomio x i ys) (productoCont xs (i+1) ys)

producto2 :: (Num a, Eq a) => Polinomio a -> Polinomio a -> Polinomio a
producto2 [] _ = []
producto2 (a:as) g = suma p q
  where p = productoPorEscalar a g
        q = 0:(producto2 as g)


--CLASE 11 ---------------------------------------------------------------------

--division :: Polinomio -> Polinomio -> (Polinomio, Polinomio)
--division p [] = ([],[])
--division p q | deg p < deg q = (0, p)
--             | otherwise = (c,r1)
--  where c = suma c1 c0
--        (c1, r1) = p1 q
--        c0 = monomio (a / b) d
--        a = last p
--        b = last q
--        p1 = resta p (producto q c0)
--        d = deg p - (deg q)

--mcdP :: Polinomio -> Polinomio -> Polinomio
--mcdP [] q = q
--mcdP p q = mcdP q r
--  where (_, r) = division p q


--monomio :: Float -> Integer -> Polinomio
--monomio a 0 = [a]
--monomio a d = 0:(monomio a (d-1))

multiplicidad :: (Num a, Eq a) => a -> Polinomio a -> Integer
multiplicidad x p | evaluar p x == 0 = 1 + (multiplicidad x (derivada p))
                  | otherwise = 0;

--P tiene raices multiples sii (P:P') != 1
--raicesMultiples :: Polinomio -> Bool
--raicesMultiples p = not(sonCoprimos p (derivada p))

--sonCoprimos :: Polinomio -> Polinomio -> Bool
--sonCoprimos p q = grado (mcdP p q) == 0

esRaizRacional :: (Num a, Eq a, Fractional a) => Polinomio a -> (Integer, Integer) -> Bool
esRaizRacional p (a, b) = evaluar p ((realToFrac a) / (realToFrac b)) == 0

raicesEnConjunto :: (Num a, Eq a, Fractional a) => Polinomio a -> [(Integer, Integer)] -> [(Integer, Integer)]
raicesEnConjunto p [] = []
raicesEnConjunto p (r:rs) | esRaizRacional p r = r:raicesEnConjunto p rs
                          | otherwise = raicesEnConjunto p rs

--candidatosRaices :: (Num a, Eq a) => Polinomio a -> [(Integer, Integer)]
--candidatosRaices p = productoCartesiano da0 dan


--Lista todos los divisores de n
listaDivisores :: Integer -> [Integer]
listaDivisores n = listaDivisoresDesde n 1

--Veo si un k es divisor de n
esDivisor n k = (mod n k) == 0

--Lista todos los divisores de n desde k
listaDivisoresDesde :: Integer -> Integer -> [Integer]
listaDivisoresDesde n 0 = listaDivisoresDesde n (1)
listaDivisoresDesde n k | k == n = [k]
                        | esDivisor n k = k : (listaDivisoresDesde n (k+1))
                        | otherwise = listaDivisoresDesde n (k+1)
