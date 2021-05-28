module FastFSA where

import Data.List(lookup)

import qualified Memoization as M

-- This is just `GenericAutomaton st sy Double'. I'm not bothering 
-- with semiring flexibility here.
type ProbAutomaton st sy = ([st], [sy], st -> Double, st -> Double, (st,sy,st) -> Double)

-- Feel free to ignore the details of this function.
makePFSA :: (Eq st, Eq sy) => Double -> ([st], [sy], [(st,Double)], [(st,Double)], [((st,sy,st),Double)]) -> ProbAutomaton st sy
makePFSA def (states, syms, starts, ends, transitions) =
    let mylookup l x = case lookup x l of {Just y -> y; Nothing -> def} in
    (states, syms, mylookup starts, mylookup ends, mylookup transitions)

pfsa :: ProbAutomaton Int Char
pfsa = makePFSA 0 ([1,2,3], ['C','V'],
                   [(1, 1.0)], 
                   [(1, 0.1)], 
                   [((1,'V',1), 0.2),
                    ((1,'C',2), 0.5),
                    ((1,'V',3), 0.2),
                    ((2,'V',1), 0.5),
                    ((2,'V',3), 0.5),
                    ((3,'C',1), 1.0)])

----------------------------------------------------------

-- Functions imported from Memoization:
--   lift0 :: x -> TableBased c v x
--   lift1 :: (x -> y) -> TableBased c v x -> TableBased c v y
--   lift2 :: (x -> y -> z) -> TableBased c v x -> TableBased c v y -> TableBased c v z
--   liftMany :: ([a] -> a) -> [TableBased c v a] -> TableBased c v a
--   type Booster a = (a -> a)
--   memoFix :: (Ord c) => Booster (c -> TableBased c a a) -> c -> a
--   memoFix2 :: (Ord c1, Ord c2) => Booster (c1 -> c2 -> TableBased (c1,c2) a a) -> c1 -> c2 -> a

-- Vanilla backward function, gets slow for very long strings
backward :: ProbAutomaton st sy -> [sy] -> st -> Double
backward m w q =
    let (states, syms, i, f, delta) = m in
    case w of
    [] -> f q
    (x:rest) -> sum (map (\q1 -> delta (q,x,q1) * backward m rest q1) states)

fastBackward :: (Ord st, Ord sy) => ProbAutomaton st sy -> [sy] -> st -> Double
fastBackward m =
    let (states, syms, i, f, delta) = m in
    M.memoFix2 (\otherWorker -> \w -> \q ->
            case w of
            [] -> M.lift0 (f q)
            (x:rest) -> let prod q1 = M.lift1 (\y -> delta (q,x,q1) * y) (otherWorker rest q1) in
                        M.liftMany sum (map prod states)
    )

