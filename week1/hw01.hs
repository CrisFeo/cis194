{-# OPTIONS_GHC -Wall #-}
module HW01 where

import Data.List

-- Decomposes an integer into a list of digits in order of least to most
-- significant.
extractDigits :: Integer -> [Integer]
extractDigits 0 = [0]
extractDigits n = n `mod` 10 : extractDigits (n `quot` 10)

-- Decomposes an integer into a list of digits in order of most to least
-- significant.
splitNumber :: Integer -> [Integer]
splitNumber 0 = [0]
splitNumber n = tail . reverse . extractDigits $ n

mapIndexedDigit :: Integer -> Integer -> (Integer, Integer)
mapIndexedDigit index digit = (index + 1, digit * (10 ^ index))

-- Take an array of integers and return the concatenation of the values
-- Example [1, 2, 3, 4] -> 1234
concatNumber :: [Integer] -> Integer
concatNumber = sum . snd . mapAccumL mapDigit 0 . reverse
  where mapDigit :: Integer -> Integer -> (Integer, Integer)
        mapDigit index digit = (index + 1, digit * (10 ^ index))

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit = last . splitNumber

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit = concatNumber . init . splitNumber

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits = reverse . splitNumber

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []         = []
doubleEveryOther [a]        = [a]
doubleEveryOther (a:b:rest) = a : b * 2 : doubleEveryOther rest

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits = sum . map (sum . splitNumber)


-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn = (== 0) . (`mod` 10) . sumDigits . doubleEveryOther . toRevDigits


-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b c = [(a,c)]
hanoi n a b c = hanoi (n-1) a c b ++ hanoi 1 a b c ++ hanoi (n-1) b a c
