{-# LANGUAGE FlexibleInstances #-}

module Assignment05 where

import Control.Applicative(liftA, liftA2, liftA3)

import SemiringFSA

data Numb = Z | S Numb deriving Show

distrib_lhs :: (Semiring v) => v -> v -> v -> v
distrib_lhs x y z = x &&& (y ||| z)

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.

----1: Writing functions that work for any semiring -------

distrib_rhs :: (Semiring v) => v -> v -> v -> v
distrib_rhs x y z = (x &&& y) ||| (x &&& z)

dotprod :: (Semiring v) => [v] -> [v] -> v
dotprod x y = case x of 
    [] -> gfalse 
    xhead:xtail -> case y of 
        [] -> gfalse
        yhead:ytail -> (xhead &&& yhead) ||| (dotprod xtail ytail)

expn :: (Semiring v) => v -> Numb -> v   
expn x n = case n of 
     Z -> gtrue
     S n' -> x &&& (expn x n')


-----2: Now for semiring-based FSA ----------

backward :: (Semiring v) => GenericAutomaton st sy v -> [sy] -> st -> v
backward m w q = let (states, syms, i, f, delta) = m in
    case w of
    [] -> f q
    (x:rest) -> gen_or (map (\q1 -> delta (q,x,q1) &&& backward m rest q1) states)


f :: (Semiring v) => GenericAutomaton st sy v -> [sy] -> v
f m w = let (states, syms, i, fin, delta) = m in 
   gen_or(map (\q0 -> (i q0) &&& (backward m w q0)) states)

----- 3: Adding the cost semiring ----------

addCost :: Cost -> Cost -> Cost
addCost x y = case x of 
        Inf -> Inf 
        TheInt x' -> case y of
            Inf -> Inf 
            TheInt y' -> TheInt (x' + y')

minCost :: Cost -> Cost -> Cost     
minCost x y = case x of 
    Inf -> y
    TheInt x' -> case y of 
        Inf -> x
        TheInt y' -> TheInt (min x' y')    

instance Semiring Cost where
    x &&& y = addCost x y
    x ||| y = minCost x y 
    gtrue = TheInt 0
    gfalse = Inf

------ 4 Adding the set-of-strings semiring ------

instance Semiring [[a]] where
  x &&& y = liftA2(\m -> \m2 -> m++m2) x y 
  x ||| y = x++y 
  gtrue = [[]]
  gfalse = []


gfsa39 :: GenericAutomaton Int Char [[Char]]
gfsa39 = makeGFSA [] ([1,2,3], ['C','V'],
                    [(1, [[]])], 
                    [(1, [[]])], 
                    [((1,'C',2), ["C"]),
                    ((1, 'V', 1), ["V"]),
                     ((1,'V',3),["V"]),
                     ((2,'V',3), ["V", "VV"]),
                     ((2,'V',1), ["V", "VV"]),
                     ((3,'C',1), [[]])])

gfsa_flap :: GenericAutomaton Int Char [[Char]] 
gfsa_flap = makeGFSA [] ([0,1, 2], ['a', 'n', 't', 'T'],
                    [(0, [[]])], 
                    [(0, [[]]), (2, ["t"]), (1, [[]])], 
                    [((0,'n',0), ["n"]),
                    ((0, 't', 0), ["t"]),
                     ((0,'a',1),["a"]),
                     ((1,'n',0), ["n"]),
                     ((1,'a',1), ["a"]),
                     ((1,'t',2), [[]]),
                     ((2, 'a', 1), ["ta", "Ta"]),
                     ((2, 'n', 0), ["tn"]),
                     ((2, 't', 0), ["tt"])])
                     
---- 5: Just when you thought it couldn’t get any more amazing . . .

gfsa7_count :: GenericAutomaton Int Char Double
gfsa7_count = makeGFSA 0.0 ([1,2,3], ['C','V'],
                         [(1, 1.0)], [(1, 1.0)], 
                         [((1,'V',1), 1.0 ),
                          ((1,'C',2),1.0 ),
                          ((1,'V',3),1.0 ),
                          ((2,'V',1),1.0 ),
                          ((2,'V',3),1.0 ),
                          ((3,'C',1), 1.0)])

gfsa7_paths :: GenericAutomaton Int Char [[Int]]
gfsa7_paths = makeGFSA [] ([1,2,3], ['C','V'],
                         [(1, [[1]])], [(1, [[]])], 
                         [((1,'V',1), [[1]]),
                          ((1,'C',2), [[2]]),
                          ((1,'V',3), [[3]]),
                          ((2,'V',1), [[1]]),
                          ((2,'V',3), [[3]]),
                          ((3,'C',1), [[1]])])

