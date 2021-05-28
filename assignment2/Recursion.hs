module Recursion where

----------------------------------------------------
-- 1. Propositional formulas

data Form = T | F | Neg Form | Cnj Form Form | Dsj Form Form deriving Show

f1 :: Form
f1 = Dsj (Neg T) (Cnj F T)

-- removeNegs = \form -> case form of {T -> T; F -> F; ....}
removeNegs :: Form -> Form
removeNegs = \form -> case form of
                      T -> T
                      F -> F
                      Neg phi -> removeNegs phi
                      Cnj phi psi -> Cnj (removeNegs phi) (removeNegs psi)
                      Dsj phi psi -> Dsj (removeNegs phi) (removeNegs psi)

-- The type `Bool' is defined under the hood something like this:
-- data Bool = False | True deriving Show

-- The function `not' is defined under the hood something like this:
-- not = \b -> case b of {True -> False; False -> True}

denotation :: Form -> Bool
denotation = \form -> case form of
                      T -> True
                      F -> False
                      Neg phi -> not (denotation phi)  -- case (denotation phi) of {True -> False; False -> True}
                      Cnj phi psi -> case (denotation phi) of {True -> denotation psi; False -> False}
                      Dsj phi psi -> case (denotation phi) of {True -> True; False -> denotation psi}

----------------------------------------------------
-- 2. A very simple recursive type

data Numb = Z | S Numb deriving Show

one = S Z
two = S one
three = S two
four = S three
five = S four

-- A non-recursive function on the Numb type. Notice how non-recursive 
-- functions like this are insensitive to distinctions between numbers 
-- that one can only see by looking beyond a fixed depth. (For example, 
-- this lessThanTwo function doesn't ``see'' the distinction between 
-- three and four.)
lessThanTwo :: Numb -> Bool
lessThanTwo = \n -> case n of
                    Z -> True
                    S n' -> case n' of {Z -> True; S n'' -> False}

-- Our first recursive function. 
-- This is equivalent to the function f in (2) on the handout.
double :: Numb -> Numb
double = \n -> case n of
               Z -> Z
               S n' -> S (S (double n'))

-- A doubling function that works the same way but on built-in integers. The idea that 
-- recursive calls apply to ``sub-parts'' of the argument doesn't come out so clearly this way.
double' :: Int -> Int
double' = \n -> case (n <= 0) of {True -> 0; False -> 2 + (double' (n-1))}

-- Notice how this function says, in effect, that Z gets ``converted'' to `False' 
-- and that S gets converted to `not'.
-- For example, `isOdd (S (S (S Z)))' is worked out as `(not (not (not False)))'.
isOdd :: Numb -> Bool
isOdd = \n -> case n of
              Z -> False
              S n' -> case (isOdd n') of {True -> False; False -> True}
                      -- (\b -> case b of {True -> False; False -> True}) (isOdd n')
                      -- not (isOdd n')

-- This is written in a way that makes it clear that this is a function 
-- which takes a number as an argument and returns a function
add :: Numb -> (Numb -> Numb)
add = \n -> case n of
            Z    -> \m -> m
            S n' -> \m -> S ((add n') m)   -- or: (add n') (S m)

-- This version is entirely equivalent to `add' but makes it look a bit 
-- more like a ``function that takes two arguments''
otherAdd :: Numb -> (Numb -> Numb)
otherAdd = \n -> \m -> case n of
                       Z    -> m
                       S n' -> S ((otherAdd n') m)   -- or: (add n') (S m)

----------------------------------------------------
-- 3. Another recursive type: lists/strings

data IntList = Empty | NonEmpty Int IntList deriving Show

-- Notice this definition, which gives a name to a list, is not interestingly 
-- different from all the other surrounding definitions, which give names to functions.
myList :: IntList
myList = NonEmpty 5 (NonEmpty 7 (NonEmpty 2 Empty))

-- This is (8) on the handout
total :: IntList -> Int
total = \l -> case l of
              Empty -> 0
              NonEmpty x rest -> x + total rest

-- This is (10) on the handout
otherTotal :: [Int] -> Int
otherTotal = \l -> case l of
                   [] -> 0
                   x:rest -> x + otherTotal rest

