--criba :: Integer -> [Integer]
--criba n = filter (\n -> (mod x (head xs) == 0)) xs
--  where (x:xs) = [2..n]


--criba :: Integer -> [Integer]
--criba n = generarCriba [2..n]

--generarCriba (x:xs) = filter ( \x -> (mod x ) )



hola xs = filter (\x -> x /= 0) $ concat (map (\x -> map (\y -> mod y x) xs) xs)


hola2 xs = map (\y -> filter (\x -> mod x y /= 0) xs) xs

cribaMejor :: Integer -> [Integer]
cribaMejor n = quitarDivisores [2..n]

quitarDivisores :: [Integer] -> [Integer]
quitarDivisores [] = []
quitarDivisores (x:xs) = x:quitarDivisores(filter (\n -> mod n x /= 0) xs)




primos :: [Int]
primos = eratostenes [2..1000000]

primos3 :: [Integer]
primos3 = quitarDivisores [2..100000]
primos2 :: [Integer]
primos2 = eratostenes2 [2..1000000] --lista infinita de números naturales

--Implementamos la criba recursivamente
criba :: [Int] -> [Int]
criba [] = []
criba (p:xs) = p : criba [x | x <- xs, x `mod` p /= 0]

--hola5 xs = map (\x -> (x:)) xs


eratostenes :: [Int] -> [Int] -- Criba de Eratóstenes (de una lista dada [2..n] te deja sólo los números primos)
eratostenes [] = []
eratostenes (x:xs) | not (null xs) && x^2 > last xs = (x:xs)
                   | otherwise = x: eratostenes [y | y <- xs, y `mod` x /= 0]
--[2..n]

eratostenes2 :: [Integer] -> [Integer] -- Criba de Eratóstenes (de una lista dada [2..n] te deja sólo los números primos)
eratostenes2 [] = []
eratostenes2 (x:xs) | x^2 > last xs = (x:xs)
                   | otherwise = x: eratostenes2 (filter (\n -> mod n x /= 0) xs)





sieve :: [Integer] -> [Integer]

sieve (0:xs) = sieve xs

sieve (n : xs) = n : sieve ( mark xs 1 n )

  where 
        mark :: [Integer] -> Integer -> Integer -> [Integer]

        mark (y:ys) k m | k == m = 0 : (mark ys 1 m)
                        | otherwise = y : (mark ys (k+1) m)

