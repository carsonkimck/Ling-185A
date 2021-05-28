module FiniteStatePart2 where

----------------------------------------------------------------------------
-- Some helper functions

-- liftA :: (a -> b) -> [a] -> [b]
-- liftA2 :: (a -> b -> c) -> [a] -> [b] -> [c]
-- liftA3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
import Control.Applicative(liftA, liftA2, liftA3)

-- nub just removes duplicate elements from a list
-- nub :: (Eq a) => [a] -> [a]
import Data.List(nub)
 
-- filter (already defined) removes from a list of elements that don't satisfy the given predicate
-- e.g. filter (\x -> x > 3) [1,2,3,4,5]   ==>   [4,5]
-- filter :: (a -> Bool) -> [a] -> [a]

----------------------------------------------------------------------------
-- Simple definitions

data SegmentCV = C | V deriving (Show,Eq)

-- This now has two type parameters: one for states, and one for symbols
type Automaton st sy = ([st], [sy], [st], [st], [(st,sy,st)])

-- FSA requiring an odd number of Cs
fsa_oddCs :: Automaton Bool SegmentCV
fsa_oddCs = ([True,False], [C,V], [False], [True], 
             [(False, C, True), (False, V, False), (True, C, False), (True, V, True)])

-- FSA requiring an even number of Vs
fsa_evenVs :: Automaton Bool SegmentCV
fsa_evenVs = ([True,False], [C,V], [False], [False], 
              [(False, C, False), (False, V, True), (True, C, True), (True, V, False)])

----------------------------------------------------------------------------
-- Basic generation (essentially the same as last week)

backward :: (Eq st, Eq sy) => Automaton st sy -> [sy] -> st -> Bool
backward m w q =
    let (states, syms, i, f, delta) = m in
    case w of
    [] -> elem q f
    (x:rest) -> or (map (\q1 -> elem (q,x,q1) delta && backward m rest q1) states)

generates :: (Eq st, Eq sy) => Automaton st sy -> [sy] -> Bool
generates m w =
    let (states, syms, i, f, delta) = m in
    or (map(\q0 -> elem q0 i && backward m w q0) states)

----------------------------------------------------------------------------
-- Intersection of FSAs

intersect :: (Eq st, Eq st', Eq sy) => Automaton st sy -> Automaton st' sy -> Automaton (st,st') sy
intersect (states, syms, i, f, delta) (states', syms', i', f', delta') =
    let newStates = liftA2 (\x -> \y -> (x,y)) states states' in
    let newI = liftA2 (\x -> \y -> (x,y)) i i' in
    let newF = liftA2 (\x -> \y -> (x,y)) f f' in
    let newSyms = nub (syms ++ syms') in    -- A bit ugly. Only common symbols will end up in newDelta.
    let candidateTransitions = liftA3 (\x -> \y -> \z -> (x,y,z)) newStates newSyms newStates in
    let isValidTransition ((q1,q1'),x,(q2,q2')) = elem (q1,x,q2) delta && elem (q1',x,q2') delta' in
    let newDelta = filter isValidTransition candidateTransitions in
    (newStates, newSyms, newI, newF, newDelta)

----------------------------------------------------------------------------
-- FSAs with epsilon transitions

-- Maybe types are pre-defined like this. You can think of them 
-- like a non-recursive list, with a maximum length of one.
-- data Maybe a = Nothing | Just a deriving (Eq,Show)

-- See (20) on the handout
type EpsAutomaton st sy = ([st], [sy], [st], [st], [(st, Maybe sy, st)])

-- See (21) on the handout
efsa_handout21 :: EpsAutomaton Int Char
efsa_handout21 = ([10,20,21,30,31,32], ['a','b'], 
                  [10], [20,30], [(10, Just 'a', 10), (10, Nothing, 20),  (10, Nothing, 30), 
                                  (20, Just 'b', 21), (21, Just 'b', 20), 
                                  (30, Just 'b', 31), (31, Just 'b', 32), (32, Just 'b', 30) ]
                 )

-- See (22) on the handout
efsa_handout22 :: EpsAutomaton Int Char
efsa_handout22 = ([0,1,2], ['a','b','c'], 
                  [0], [2], [(0, Just 'a', 0), 
                             (0, Nothing,  1), 
                             (1, Just 'b', 1), 
                             (1, Nothing,  2), 
                             (2, Just 'c', 2)]
                 )

-- One more epsilon-FSA
efsa_xyz :: EpsAutomaton Int Char
efsa_xyz = ([0,1], ['x','y','z'], [0], [1], [(0, Just 'x', 0), (0, Just 'y', 1), (0, Nothing, 1), (1, Just 'z', 1)])

-- See (23) on the handout. Feel free to ignore the implementation of this.
epsilonClosure :: (Eq st, Eq sy) => [(st, Maybe sy, st)] -> st -> [st]
epsilonClosure delta q =
    let outgoingEpsilons q' = filter (\(q1,x,q2) -> q1 == q' && x == Nothing) delta in
    let oneStepFrom q' = map (\(q1,x,q2) -> q2) (outgoingEpsilons q') in
    let update qs = nub (qs ++ (concat (map oneStepFrom qs))) in
    until (\qs -> update qs == qs) update [q]

-- See (24) on the handout.
removeEpsilons :: (Eq st, Eq sy) => EpsAutomaton st sy -> Automaton st sy
removeEpsilons (states, syms, i, f, delta) =
    let validTransition (q1,x,q2) = or (map (\q' -> elem q' (epsilonClosure delta q1) && elem (q', Just x, q2) delta) states) in
    let newDelta = filter validTransition (liftA3 (\x -> \y -> \z -> (x,y,z)) states syms states) in
    let canReachEnd q = or (map (\q' -> elem q' f) (epsilonClosure delta q)) in
    let newEnds = filter canReachEnd states in
    (states, syms, i, newEnds, newDelta)

----------------------------------------------------------------------------
-- Regular expressions

-- See (25) on the handout.
data RegExp a = Lit a | Alt (RegExp a) (RegExp a) | Concat (RegExp a) (RegExp a) 
              | Star (RegExp a) | ZeroRE | OneRE
              deriving (Eq,Show)

-- Some example regular expressions, as in (26)
re_26a = Alt (Lit 'a') (Lit 'b')
re_26b = Concat re_26a (Lit 'c')
re_26c = Star re_26b

-- Some names that make the `denotation' function below a bit easier to read, 
-- by giving clues about which lists are representing sets and which lists 
-- are representing strings.
union x y = x ++ y
emptyString = []
emptySet = []

-- The `denotation' function here closely follows the definition in (27) on the handout. 
-- This function ``is correct'', but doesn't deal very sensibly with infinite sets. 
-- You can use the `take' function (like `prefix' from an earlier HW exercise) to look 
-- at a few elements from an infinite denotation without losing control of your terminal:
--      *FiniteStatePart2> take 5 (denotation (Star (Lit C)))
--      [[],[C],[C,C],[C,C,C],[C,C,C,C]]
-- But no many how far you look you'll never actually find a string of Vs in a case like 
-- the following, because of the ``dumb'' way the Star interacts with Alt:
--      *FiniteStatePart2> take 5 (denotation (Star (Alt (Lit C) (Lit V))))
--      [[],[C],[C,C],[C,C,C],[C,C,C,C]]
-- These details could be tidied up but we won't worry about it for our purposes.
denotation :: RegExp a -> [[a]]             -- The result type here is a ``set'' of strings, using symbols of type a
denotation regexp = case regexp of
                    Lit x -> [ [x] ]        -- A ``set'' containing a single string that consists of a single symbol
                    Alt r1 r2 -> union (denotation r1) (denotation r2)
                    Concat r1 r2 -> liftA2 (\u -> \v -> u ++ v) (denotation r1) (denotation r2)
                    Star r -> emptyString : (liftA2 (\u -> \v -> u ++ v) (denotation r) (denotation (Star r)))
                    ZeroRE -> emptySet      -- A ``set'' containing zero strings
                    OneRE -> [emptyString]  -- A ``set'' containing a single string whose length is zero



