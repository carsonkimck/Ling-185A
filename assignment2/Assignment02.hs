module Assignment02 where

-- Imports everything from the Recursion module. 
import Recursion 

-- Imports just a few things that we have seen from the standard Prelude module. 
-- (If there is no explicit 'import Prelude' line, then the entire Prelude 
-- module is imported. I'm restricting things here to a very bare-bones system.)
import Prelude((+), (-), (*), (<), (>), not, Bool(..), Int, Show)

-- Just to give us one more type to play around with.
data Shape = Rock | Paper | Scissors deriving Show


------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line. ---



------- Recursive functions on the Numb type-----------

mult :: Numb -> (Numb -> Numb)
mult = \n -> case n of 
    Z -> \m -> Z
    S n' -> \m -> case m of 
        Z -> Z 
        S m  -> add n (mult m n)

sumUpTo :: Numb -> Numb
sumUpTo = \n -> case n of  
    Z -> Z
    S n' -> add n (sumUpTo n')


equal :: Numb -> (Numb -> Bool)
equal = \n -> \m -> case n of
    Z -> case m of {Z -> True; S m' -> False}
    S n' ->  case m of 
        Z -> False
        S m' -> equal n' m'

bigger :: Numb -> (Numb -> Numb)
bigger = \n -> \m -> case n of 
    Z -> m
    S n' -> case m of 
        Z -> n
        S m' -> S (bigger n' m')

        
---------Recursive functions on lists--------


count :: (Int -> Bool) -> ([Int] -> Numb) 
count = \f -> \l -> case l of 
    [] -> Z
    x:rest -> case f x of {True -> S (count f rest);  False -> count f rest} 


listOf :: Numb -> (Shape -> [Shape])
listOf = \n -> \s -> case n of  
        Z -> []
        S n' -> s : (listOf n' s) 

addToEnd :: Shape -> ([Shape] -> [Shape])
addToEnd = \s -> \l -> case l of 
    [] -> [s]
    x:rest -> x: (addToEnd s rest)

remove :: (Int -> Bool) -> ([Int] -> [Int])
remove = \f -> \l -> case l of 
    [] -> []
    x:rest -> case f x of {True -> (remove f rest); False -> x:(remove f rest)}

prefix :: Numb -> ([Shape] -> [Shape])
prefix = \n -> \l -> case n of 
     Z -> []
     S n' -> case l of 
         [] -> []
         x: rest ->  (x: prefix n' rest)


---- Recursive functions on the Form type ------
countNegs :: Form -> Numb 
countNegs = \f -> case f of
    T -> Z 
    F -> Z 
    Neg f' -> S (countNegs f')
    Cnj phi psi -> add (countNegs phi) (countNegs psi)
    Dsj phi psi -> add (countNegs phi) (countNegs psi)


depth :: Form -> Numb
depth = \f -> case f of 
    T -> S(Z)
    F -> S(Z) 
    Neg f' -> S(depth f')
    Cnj phi psi -> bigger (S (depth phi)) ( S (depth psi))
    Dsj phi psi -> bigger (S (depth phi)) ( S (depth psi))


leftmostLeaf :: Form -> Form
leftmostLeaf = \f -> case f of 
    T -> T 
    F -> F 
    Neg f' -> leftmostLeaf (f')
    Cnj phi psi -> leftmostLeaf(phi)
    Dsj phi psi -> leftmostLeaf(phi)

