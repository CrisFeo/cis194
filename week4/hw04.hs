{-# OPTIONS_GHC -Wall #-}
module HW04 where

import Data.List

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [1]

-- Exercise 2 ----------------------------------------

trimZeros :: (Num a, Eq a) => [a] -> [a]
trimZeros = reverse . trimLeadingZeros . reverse
  where trimLeadingZeros (0:rest) = trimLeadingZeros rest
        trimLeadingZeros list     = list

instance (Num a, Eq a) => Eq (Poly a) where
    (==) (P coeffA) (P coeffB) = trimZeros coeffA == trimZeros coeffB
 
-- Exercise 3 -----------------------------------------

renderTerm :: (Num a, Eq a, Show a) => Int -> a -> [Char]
renderTerm _      0     = []
renderTerm 0      coeff = show coeff
renderTerm 1      1     = "x"
renderTerm 1      (-1)  = "-x"
renderTerm 1      coeff = show coeff ++ "x"
renderTerm degree 1     = "x^" ++ show degree
renderTerm degree (-1)  = "-x^" ++ show degree
renderTerm degree coeff = show coeff ++ "x^" ++ show degree

joinTerms :: [[Char]] -> [Char]
joinTerms = concat . intersperse " + " . filter (/=[]) . reverse

instance (Num a, Eq a, Show a) => Show (Poly a) where
  show (P coeff)
    | trimZeros coeff == [] = "0"
    | otherwise             = joinTerms
                              . zipWith renderTerm [0..]
                              . trimZeros $ coeff

-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P coeffA) (P coeffB) = P (zipWith (+) coeffA coeffB)

-- Exercise 5 -----------------------------------------

-- A term is a pair whose first element is the exponent of the polynomial term
-- being represented and whose second element is the coefficient of that term.
type Term a = (Int, a)

toTerms :: Num a => Poly a -> [Term a]
toTerms (P coeff) = zip [0..] coeff

toPoly :: Num a => [Term a] -> Poly a
toPoly = P . map (snd) . foldr (fillList) []
  where fillList :: Num a => Term a -> [Term a] -> [Term a]
        fillList nextTerm [] = [nextTerm]
        fillList nextTerm@(nE,_) terms@((lE,_):_)
          | nE == lE-1 = nextTerm:terms
          | otherwise  = fillList nextTerm $ (lE-1,0):terms

-- Given two lists of polynomial terms returns the unreduced result of multiplying
-- them together
multiplyPolyTerms :: Num a => [Term a] -> [Term a] -> [Term a]
multiplyPolyTerms a b = concatMap (multiplyAllWith b) a
  where multiplyAllWith :: Num a => [Term a] -> Term a -> [Term a]
        -- Multiply a list of terms with a single term
        multiplyAllWith terms term = map (multiplyTerms term) terms
        multiplyTerms :: Num a => Term a -> Term a -> Term a
        -- Multiply two terms together. Exponents add, coefficients multiply
        multiplyTerms (eA, cA) (eB, cB) = (eA + eB, cA * cB)
        
-- Sums up the coefficients of polynomial Terms with the same exponent
reducePolyTerms :: Num a => [Term a] -> [Term a]
reducePolyTerms = foldl (sumWithHead) [] . sortOn (negate . fst)
  where sumWithHead :: Num a => [Term a] -> Term a -> [Term a]
        -- Sum a term with the head of a list if exponents match. Otherwise append
        sumWithHead []    b = [b]
        sumWithHead (a:l) b
          | fst a == fst b  = (fst a, snd a + snd b):l
          | otherwise       = b:a:l

times :: Num a => Poly a -> Poly a -> Poly a
times a b = toPoly . reducePolyTerms . multiplyPolyTerms (toTerms a) $ (toTerms b)

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+)                   = plus
    (*)                   = times
    negate (P terms)      = P . map (negate) $ terms
    fromInteger           = P . (:[]) . fromInteger
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP poly xValue = sum . map (applyTerm xValue) . toTerms $ poly
  where applyTerm :: Num a => a -> Term a -> a
        applyTerm xVal (e,c) = c*(xVal^e)

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv n d
      | n < 0     = undefined
      | n == 0    = d
      | otherwise = deriv . nderiv (n-1) $ d

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv poly = toPoly . map (derivTerm) . toTerms $ poly
      where derivTerm :: Num b => Term b -> Term b
            derivTerm (e,c)
              | e < 0     = (e-1,0)
              | otherwise = (e-1, c * (fromIntegral e))
