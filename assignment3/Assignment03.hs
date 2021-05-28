module Assignment03 where

-- Imports everything from the FiniteState module
import FiniteState

-- Another type we'll use as symbols for some FSAs
data SegmentPKIU = P | K | I | U | MB deriving (Show,Eq)

-- A list-like type that will be useful for computing forward values
data SnocList a = ESL | (SnocList a) ::: a deriving Show

-- The word ``hello'' encoded as a snoc list of characters
sl :: SnocList Char
sl = ((((ESL ::: 'h') ::: 'e') ::: 'l') ::: 'l') ::: 'o'

-- Checks that all states and symbols mentioned in the transition 
-- table (i.e. delta) come from the provided lists of states and symbols.
fsaSanityCheck :: (Eq a) => Automaton a -> Bool
fsaSanityCheck m =
    let (states, syms, i, f, delta) = m in
    let validTransition (q1,x,q2) = elem q1 states && elem x syms && elem q2 states in
    and (map validTransition delta)

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.

------- Problem 1: Encoding finite-state automata formally -------------

fsa_countVs :: Automaton SegmentCV 
fsa_countVs = ([54, 73, 21, 38], [C, V], [54], [38], [(54, C, 54), 
                                                    (54, V, 73), 
                                                    (73, C, 73),
                                                    (73, V, 21), 
                                                    (21, C, 21),
                                                    (21, V, 38), 
                                                    (38, C, 38), 
                                                    (21, V, 54)])  


-------- Problem 2:  “Snoc lists” -------------

addToFront :: a -> SnocList a -> SnocList a
addToFront a sl = case sl of 
    ESL -> ESL:::a
    rest:::x -> addToFront a rest:::x


toSnoc :: [a] -> SnocList a
toSnoc list = case list of 
    [] -> ESL 
    x:rest -> addToFront x (toSnoc rest) 

----- Problem 3: Forward values------------

forward :: (Eq a) => Automaton a -> SnocList a -> State -> Bool 
forward m w q = let (states, sym, i, f, delta) = m in 
                 case w of 
                 ESL -> elem q i 
                 rest:::x -> or (map (\q' -> elem (q', x, q) delta && forward m rest q') states)


generates2 :: (Eq a) => Automaton a -> [a] -> Bool
generates2 m w = let (states, syms, i, f, delta) = m in 
        or (map (\q0 -> elem q0 f && forward m (toSnoc w) q0) states)


----- Problem 4: Designing finite-state automata-------------

fsa_twoCs :: Automaton SegmentCV
fsa_twoCs = ([1, 2, 3], [C, V], [1], [3], [(1, C, 1), 
                                                    (1, V, 1), 
                                                    (1, C, 2),
                                                    (2, C, 3), 
                                                    (2, C, 2),
                                                    (2, V, 2),
                                                    (3, C, 3),
                                                    (3, V, 3)])


fsa_thirdC :: Automaton SegmentCV  
fsa_thirdC = ([1, 2, 3, 4, 5, 6, 7, 8], [C, V], [1], [8], [(1, C, 2),
                                                            (2, C, 3),
                                                            (2, V, 4),
                                                            (4, C, 8),
                                                            (2, C, 3),
                                                            (3, C, 8),
                                                            (8, C, 8),
                                                            (8, V, 8),
                                                            (1, V, 5),
                                                            (5, C, 6),
                                                            (6, C, 8),
                                                            (5, V, 7),
                                                            (7, C, 8)])
                                                    
fsa_thirdlastC :: Automaton SegmentCV
fsa_thirdlastC = ([1, 2, 3, 4, 5, 6, 7], [C, V], [1], [4], [(1, C, 1),
                                                            (1, V, 1),
                                                            (1, C, 1),
                                                            (1, C, 2),
                                                            (2, C, 3),
                                                            (3, C, 4),
                                                            (2, V, 5),
                                                            (5, V, 4),
                                                            (2, C, 6),
                                                            (6, V, 4),
                                                            (2, V, 7),
                                                            (7, C, 4)])
                                                    
                                                    
fsa_oddEven :: Automaton SegmentCV
fsa_oddEven = ([1, 2, 3, 4], [C, V], [1], [4], [(1, V, 2),
                                                (2, V, 1),
                                                (2, C, 3),
                                                (3, C, 2),
                                                (3, V, 4),
                                                (4, V, 3),
                                                (4, C, 1),
                                                (1, C, 4)])

fsa_harmony :: Automaton SegmentPKIU
fsa_harmony = ([1, 2, 3], [P, K, I, U, MB], [1], [1, 2, 3], [(1, P, 1),
                                                         (1, K, 1),
                                                         (1, MB, 1),
                                                         (1, U, 2),
                                                         (2, P, 2),
                                                         (2, K, 2),
                                                         (2, U, 2),
                                                         (2, MB, 1),
                                                         (1, I, 3),
                                                         (3, P, 3),
                                                         (3, K, 3),
                                                         (3, I, 3),
                                                         (3, MB, 1)])

fsa_MBU :: Automaton SegmentPKIU
fsa_MBU = ([1, 2], [P, K, I, U, MB], [1], [1, 2], [(1, P, 1),
                                                    (1, K, 1),
                                                    (1, I, 1),
                                                    (1, MB, 2),
                                                    (2, P, 2),
                                                    (2, U, 2),
                                                    (2, K, 2),
                                                    (2, I, 2)])


fsa_adjacentMBU :: Automaton SegmentPKIU
fsa_adjacentMBU = ([1, 2, 3], [P, K, I, U, MB], [1], [1, 3], [(1, P, 1),
                                                     (1, K, 1),
                                                     (1, I, 1),
                                                     (1, MB, 1),
                                                     (1, MB, 2),
                                                    (2, U, 3),
                                                    (3, P, 3),
                                                    (3, K, 3),
                                                    (3, I, 3),
                                                    (3, MB, 2)])