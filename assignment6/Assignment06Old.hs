{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
{-# LANGUAGE FlexibleInstances #-}

module Assignment06 where

import Control.Applicative(liftA, liftA2, liftA3)

import ContextFree

import qualified Memoization as M

data Tree nt t = Leaf nt t | NonLeaf nt (Tree nt t) (Tree nt t) deriving Show

tree1 :: Tree Cat String
tree1 = NonLeaf VP (NonLeaf VP (Leaf V "watches") (Leaf NP "spies"))
                   (NonLeaf PP (Leaf P "with") (Leaf NP "telescopes"))

tree2 :: Tree Cat String
tree2 = NonLeaf VP (Leaf V "watches")
                   (NonLeaf NP (Leaf NP "spies") (NonLeaf PP (Leaf P "with") (Leaf NP "telescopes")))

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.

-----  1 Leftmost derivations ------ 

-- Helper function for treeToDeriv, returns the root nonterminal of a subtree 
rootCategory :: Tree nt t -> nt  
rootCategory tree = case tree of 
    Leaf x y -> x
    NonLeaf nonT left right -> nonT

treeToDeriv :: Tree nt t -> [RewriteRule nt t]
treeToDeriv tree = case tree of 
    Leaf x y -> [TRule x y]
    NonLeaf nonT leftTree rightTree -> [NTRule nonT ((rootCategory leftTree), (rootCategory rightTree))] ++ treeToDeriv leftTree ++ treeToDeriv rightTree

--- 2 Inside, outside and memoization ---- 

fastInside :: (Ord nt, Ord t, Semiring v) => GenericCFG nt t v -> [t] -> nt -> v
fastInside cfg str' n' =
    let (nts, ts, i, r) = cfg in
    let workerBooster otherWorker str n = 
            case str of
                [] -> M.lift0 (gfalse)
                (x:[]) -> M.lift0 (r (TRule n x))
                (x:y:rest2) -> let conj i ld rd = M.liftMany gen_and [M.lift0 (r (NTRule n (ld,rd))), 
                                                                      otherWorker (take i str) ld, 
                                                                      otherWorker (drop i str) rd]

                                in M.liftMany gen_or (liftA3 conj [1 .. (length str - 1)] nts nts)
    in (M.memoFix2 workerBooster) str' n' 


f :: (Ord nt, Ord t, Semiring v) => GenericCFG nt t v -> [t] -> v
f cfg str = let (nts, ts, i, r) = cfg in 
    gen_or (map (\q -> i q &&& fastInside cfg str q) nts)


outside :: (Ord nt, Ord t, Semiring v) => GenericCFG nt t v -> ([t],[t]) -> nt -> v
outside cfg (ys,zs) n = let (nts, ts, i, r) = cfg in
    case (null ys && null zs) of
        True -> i n 
        False -> (let conjR i p right = r (NTRule p (n, right)) &&& outside cfg (ys, (drop i zs)) p  &&& fastInside cfg (take i zs) right 
                in 
                gen_or (liftA3 conjR [1 .. (length zs)] nts nts)) ||| 
                (let conjL i p left = r (NTRule p (left, n)) &&& outside cfg ((take i ys), zs) p &&& fastInside cfg (drop i ys) left 
                in gen_or (liftA3 conjL [0 .. (length ys -1 )] nts nts))


fastOutside :: (Ord nt, Ord t, Semiring v) => GenericCFG nt t v -> ([t],[t]) -> nt -> v
fastOutside cfg (ys',zs') n' = 
    let (nts, ts, i, r) = cfg in
    let workerBooster otherWorker (ys, zs) n = 
            case (null ys && null zs) of
                True -> M.lift0 (i n) 
                False -> M.liftMany gen_or [let conjR i p right = M.liftMany gen_and [M.lift0(r (NTRule p (n, right))), 
                                                                                otherWorker (ys, (drop i zs)) p,
                                                                                M.lift0(fastInside cfg (take i zs) right)]

                                           in M.liftMany gen_or (liftA3 conjR [1 .. (length zs)] nts nts), 

                                           let conjL i p left = M.liftMany gen_and [M.lift0 (r (NTRule p (left, n))), 
                                                                                    otherWorker ((take i ys), zs) p, 
                                                                                    M.lift0 (fastInside cfg (drop i ys) left)] 
                                            in M.liftMany gen_or (liftA3 conjL [0 .. (length ys -1 )] nts nts)]
    in (M.memoFix2 workerBooster) (ys', zs') n'


----- 3 Retrieving trees --------

insideTrees :: GenericCFG nt t Double -> [t] -> nt -> [(Double, Tree nt t)]
insideTrees cfg str n = 
    let (nts, ts, i, r) = cfg in
        case str of 
            [] -> []
            (x:[]) -> let prob = (r (TRule n x)) in case prob of 
                0.0 -> []
                _ -> [(prob, Leaf n x)]
            --(x:y:rest) -> let conj i ld rd = insideTrees cfg ((take i str)) ld &&& insideTrees cfg (drop i str) rd in 
                
              --  liftA2 (x -> y -> x * y) 


        

getDoubles :: [(Double, Tree nt t)] -> [Double]
getDoubles list = case list of
    [] -> []
    (x, y):rest -> [x] ++ getDoubles(rest)


getTrees :: [(Double, Tree nt t)] -> [Tree nt t]
getTrees list = case list of 
    [] -> []
    (x, y):rest -> [y] ++ getTrees(rest)




        
            







--inside :: (Semiring v) => GenericCFG nt t v -> [t] -> nt -> v
--inside cfg str n =
  --  let (nts, ts, i, r) = cfg in
    --case str of
    --[] -> gfalse
    --(x:[]) -> r (TRule n x)
   -- (x:y:rest2) -> let conj i ld rd = r (NTRule n (ld,rd)) &&& 
                                        --inside cfg (take i str) ld &&& 
                                        --inside cfg (drop i str) rd
     --              in
       --            gen_or (liftA3 conj [1 .. (length str - 1)] nts nts)
