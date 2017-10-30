module Catedra where
import Data.Char

type Set a = [a]
type Clpub = (Integer, Integer) -- Clave (e, n)
type Clpri = (Integer, Integer) -- Clave (d, n)
type Cifrado = [Integer]
type Mensaje = [Char]

aEnteros :: [Char] -> [Integer]
aEnteros = map (toInteger . ord)

aChars :: [Integer] -> [Char]
aChars = map (chr . fromInteger)
