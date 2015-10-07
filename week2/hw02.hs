{-# OPTIONS_GHC -Wall #-}
module HW02 where


-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches a b = length . filter tupleEq $ zip a b
  where tupleEq :: (Peg, Peg) -> Bool
        tupleEq = uncurry (==)


-- Exercise 2 -----------------------------------------

-- For each possible peg color, count how many times it occurs in the given code
countColors :: Code -> [Int]
countColors code = map countColorInCode colors
  where countColorInCode :: Peg -> Int
        countColorInCode color = length $ filter (color==) code


-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches a b = sum $ zipWith min (countColors a) (countColors b)


-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove secret guess = Move guess exact_matches inexact_matches
  where exact_matches   = exactMatches secret guess
        inexact_matches = (matches secret guess) - exact_matches

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent move@(Move guess _ _) secret = move == getMove secret guess

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes move codes = filter (isConsistent move) codes

-- Exercise 6 -----------------------------------------

allCodes' :: Int -> [Code] -> [Code]
allCodes' 0 codes = codes
allCodes' n [] = allCodes' (n-1) . map (:[]) $ colors
allCodes' n codes = (allCodes' $ n-1) . prependAll colors $ codes
  where prependAll :: [Peg] -> [Code] -> [Code]
        prependAll elts lists = concatMap (\ list -> map (:list) elts) $ lists

allCodes :: Int -> [Code]
allCodes n = allCodes' n []

-- Exercise 7 -----------------------------------------

type Secret = Code
type Guess  = Code

c1 :: Secret
c1 = [Red, Blue]

c2 :: Guess
c2 = [Red, Red]

c3 :: Guess
c3 = [Yellow, Yellow]

c4 :: Guess
c4 = [Red, Yellow]

getNextMove :: Secret -> [Guess] -> (Move, [Guess])
getNextMove secret guesses = (getMove secret $ head guesses, tail guesses)

solve' :: Secret -> [Guess] -> [Move]
solve' _ []              = undefined
solve' secret guesses
  | secret == nextGuess  = [nextMove]
  | otherwise            = nextMove : solve' secret validGuesses
  where (nextMove, nextGuesses) = getNextMove secret guesses
        (Move nextGuess _ _)    = nextMove
        validGuesses            = filterCodes nextMove nextGuesses

solve :: Secret -> [Move]
solve secret = solve' secret (allCodes . length $ secret)

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
