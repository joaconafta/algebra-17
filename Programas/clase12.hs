--map
mapear f [] = []
mapear f (x:xs) = f x : mapear f xs

cuadrado i = i^2

cubo i = i^3

trasladar i = i+1

--filter
filtro p [] = []
filtro p (x:xs) | p x = x : filtro p xs
                | otherwise = filtro p xs


divisores :: Integer -> [Integer]
divisores n = filter esDivisor [1..n]
  where esDivisor y = mod n y == 0

--foldl/foldr
reducir :: (a -> b -> b) -> b -> [a] -> b
reducir f z [] = z
reducir f z (x:xs) = f x (reducir f z xs)

--zip
zipear :: [a] -> [b] -> [(a, b)]
zipear _ [] = []
zipear [] _ = []
zipear (x:xs) (l:ls) = (x, l) : zipear xs ls

zipearCon :: (a -> b -> c) -> [a] -> [b] -> [c]
zipearCon f _ [] = []
zipearCon f [] _ = []
zipearCon f (x:xs) (l:ls) = f x l : zipearCon f xs ls

zipear2 = zipearCon (\x y -> (x, y))

sumaVectorial = zipearCon (+)

derivar p = tail(zipearCon (*) p [0..])

cartesiano :: [a] -> [b] -> [(a, b)]
cartesiano xs ys = concat (map (\y -> map (\x -> (y, x)) ys) xs)

listas :: Integer -> [a] -> [[a]]
listas 0 xs = [[]]
listas k xs = concat $ map(\i -> map (i:) (listas (k-1) xs)) xs
--listas k xs = concat (map(\i -> map (i:) (listas (k-1) xs)) xs)

permutar :: [a] -> [[a]]
permutar [] = [[]]
permutar xs = concat $ map(\i -> map (i:) (permutar xs)) xs
