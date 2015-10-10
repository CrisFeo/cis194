module HW03 where

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop = 
    Plus     
  | Minus    
  | Times    
  | Divide   
  | Gt
  | Ge       
  | Lt  
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement       
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement        
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1 -----------------------------------------

extend :: State -> String -> Int -> State
extend existingState variableName variableValue = retrieveVariable
  where retrieveVariable :: String -> Int
        retrieveVariable name
          | name == variableName = variableValue
          | otherwise            = existingState name

empty :: State
empty var = 0

-- Exercise 2 -----------------------------------------

evalE :: State -> Expression -> Int
evalE state (Var name)  = state name
evalE state (Val value) = value
evalE state (Op exprA bop exprB)
  | bop == Plus   = valueA + valueB
  | bop == Minus  = valueA - valueB
  | bop == Times  = valueA * valueB
  | bop == Divide = valueA `div` valueB
  | bop == Gt     = if valueA >  valueB then 1 else 0
  | bop == Ge     = if valueA >= valueB then 1 else 0
  | bop == Lt     = if valueA <  valueB then 1 else 0
  | bop == Le     = if valueA <= valueB then 1 else 0
  | bop == Eql    = if valueA == valueB then 1 else 0
    where valueA = evalE state exprA
          valueB = evalE state exprB

-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Assign a b)   = DAssign a b
desugar (Incr a)       = DAssign a $ Op (Var a) Plus (Val 1)
desugar (If a b c)     = DIf a (desugar b) (desugar c)
desugar (While a b)    = DWhile a (desugar b)
desugar (For a b c d)  = desugar $ Sequence a (While b $ Sequence d c)
desugar (Sequence a b) = DSequence (desugar a) (desugar b)
desugar (Skip)         = DSkip

--For      Statement  Expression Statement Statement

-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple state statement =
  case statement of
   DAssign a b   -> extend state a (evalE state b)
   DIf a b c     -> if evalE state a /= 0 then
                       evalSimple state b
                    else
                      evalSimple state c
   DWhile a b    -> if evalE state a /= 0 then
                      evalSimple (evalSimple state b) statement
                    else
                      state
   DSequence a b -> evalSimple (evalSimple (state) a) b
   DSkip         -> state


run :: State -> Statement -> State
run state statement = evalSimple state (desugar statement)

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l


basicSkip :: Statement
basicSkip = Skip

basicAssign :: Statement
basicAssign = Assign "A" (Val 1)

basicSequence :: Statement
basicSequence = Sequence (Assign "A" $ Val 1) (Assign "B" $ Val 2)

basicIncr :: Statement
basicIncr = Sequence (Assign "A" $ Val 1) (Incr "A")

basicIf :: Statement
basicIf = If (Op (Var "A") Eql (Val 4))
             (Assign "B" $ Val 1)
             (Assign "B" $ Val 2)

basicWhile :: Statement
basicWhile = While (Op (Var "A") Lt (Val 3))
                   (Incr "A")


{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]
