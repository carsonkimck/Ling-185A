module FixedPoint where

type Booster a = a -> a

fix :: Booster a -> a    -- (a -> a) -> a
fix f = (let x = f x in x)

data Numb = Z | S Numb deriving Show

-- double :: Numb -> Numb
-- double n = case n of
--            Z -> Z
--            S n' -> S (S (double n'))

doubleBooster :: Booster (Numb -> Numb)     -- (Numb -> Numb) -> (Numb -> Numb)
doubleBooster otherDouble = \n -> case n of
                                  Z -> Z
                                  S n' -> S (S (otherDouble n'))

double :: Numb -> Numb
double = fix doubleBooster

----------------------------------------------------

-- factorial :: Int -> Int
-- factorial n = case (n <= 1) of
--               True -> 1
--               False -> n * factorial (n-1)

factorialBooster :: Booster (Int -> Int)    -- (Int -> Int) -> (Int -> Int)
factorialBooster otherFactorial = \n -> case (n <= 1) of
                                        True -> 1
                                        False -> n * otherFactorial (n-1)

factorial :: Int -> Int
factorial = fix factorialBooster

