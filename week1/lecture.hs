-- Pattern Matching
sumtorial :: Integer -> Integer
sumtorial 0 = 0
sumtorial n = n + sumtorial (n - 1)


-- Guards
hailstone :: Integer -> Integer
hailstone n
  | n `mod` 2 == 0 = n `div` 2
  | otherwise      = (3 * n) + 1


-- Pairs
sumPair :: (Int, Int) -> Int
sumPair (x, y) = x + y


-- Multiple Arguments (Curried)
intTrain :: Int -> Int -> Int -> [Int]
intTrain x y z = [x, y, z]


-- Lists: Concatenation
listConcat = [1, 2, 3, 4] ++ [1, 2, 3, 4]


-- Lists: Prepending
listPrepend = 1 : [2, 3, 4]


-- List: Access
listIndex = [1, 2, 3, 4] !! 2
listHead = head [1, 2, 3, 4]
listTail = tail [1, 2, 3, 4]
listLast = last [1, 2, 3, 4]
listInit = init [1, 2, 3, 4]


-- List: Properties
listLength = length [1, 2, 3, 4]
listNull = null [1, 2, 3, 4]


-- List: Comprehension
listComprehension  = [x * 2 | x <- [1..10]]
listFilter = [x | x <- [1..10], x `mod` 2 == 0]


-- List Function
intListLength :: [Integer] -> Integer
intListLength []     = 0
intListLength (_:xs) = 1 + intListLength xs

sumEveryTwo :: [Integer] -> [Integer]
sumEveryTwo []       = []
sumEveryTwo (x:[])   = [x]
sumEveryTwo (x:y:zs) = (x + y) : sumEveryTwo zs
