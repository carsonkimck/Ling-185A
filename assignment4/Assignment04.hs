module Assignment04 where

import Prelude hiding (Either(..))

import Control.Applicative(liftA, liftA2, liftA3)
import Data.List(nub)

import FiniteStatePart2

---------------------------------------
-- Setup for section 1

type SLG sy = ([sy], [sy], [sy], [(sy,sy)])
data ConstructedState sy = ExtraState | StateForSymbol sy deriving (Eq, Show)

slg1 :: SLG SegmentCV
slg1 = ([C,V], [C], [V], [(C,C),(C,V),(V,V)])

slg2 :: SLG Int
slg2 = ([1,2,3], [1,2,3], [1,2,3], [(1,1),(2,2),(3,3),(1,2),(2,1),(1,3),(3,1)])

---------------------------------------
-- Setup for section 2

data Either a b = First a | Second b deriving (Show,Eq)

re1 :: RegExp Char
re1 = Concat (Alt (Lit 'a') (Lit 'b')) (Lit 'c')

re2 :: RegExp Char
re2 = Star re1

re3 :: RegExp Int
re3 = Star (Concat ZeroRE (Lit 3))

re4 :: RegExp Int
re4 = Concat (Alt (Lit 0) (Lit 1)) (Star (Lit 2))

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--------Write all your code below this line.


----- 1.1 ------ Recognizing strings generated by an SLG --------------

generatesSLG :: (Eq sy) => SLG sy -> [sy] -> Bool
generatesSLG m w = let (alphabet, i, f, bigrams) = m in 
    case w of 
        [] -> False 
        x:rest -> (elem x i) && (backwardSLG m w x)


backwardSLG :: (Eq sy) => SLG sy -> [sy] -> sy -> Bool
-- m is the grammar, w 
backwardSLG m w symbol =  
    let (alphabet, i, f, bigrams) = m in 
        case w of 
            [] -> elem symbol f
            x:rest -> (elem (symbol, x) bigrams) && (backwardSLG m rest x)

---- 1.2 Conversion to FSAs -------------------

slgToFSA :: SLG sy -> Automaton (ConstructedState sy) sy
slgToFSA (alphabet, i, f, bigrams) =
    let states = (map(\q -> StateForSymbol q) alphabet) in 
    let symbols = alphabet in 
    let start = ExtraState in 
    let finish = map(\q -> StateForSymbol q) f in             
    let delta = map (\q -> (ExtraState, q, StateForSymbol q)) i in
    let delta' = map (\(q1, q2) -> (StateForSymbol q1, q2, StateForSymbol q2)) bigrams in 
        ([ExtraState] ++ states, symbols, [ExtraState], finish, delta ++ delta')

  
---- 2 --- Converting regular expressions into Epsilon-FSAs

unionFSAs :: (Eq sy) => EpsAutomaton st1 sy -> EpsAutomaton st2 sy -> EpsAutomaton (Either st1 st2) sy
unionFSAs m m' = let (mstates, msymbols, mstart, mfinish, mdelta) = m in let (m'states, m'symbols, m'start, m'finish, m'delta) = m' in  
    let states = map(\q -> First q) mstates in 
    let states2 = map(\q -> Second q) m'states in 
    let symbols =  msymbols in
    let symbols2 =  m'symbols in 
    let start = map(\q -> First q) mstart in 
    let start2 = map (\q -> Second q) m'start in 
    let finish = map(\q -> First q) mfinish in 
    let finish2 = map (\q -> Second q) m'finish in 
    let delta = map (\(q1, x, q2) -> (First q1, x, First q2)) mdelta in
    let delta2 = map (\(q1, x, q2) -> (Second q1, x, Second q2)) m'delta in
        (states ++ states2, nub (symbols ++ symbols2), start ++ start2, finish ++ finish2, (delta ++ delta2))

concatFSAs :: (Eq sy) => EpsAutomaton st1 sy -> EpsAutomaton st2 sy -> EpsAutomaton (Either st1 st2) sy
concatFSAs m m' = let (mstates, msymbols, mstart, mfinish, mdelta) = m in let (m'states, m'symbols, m'start, m'finish, m'delta) = m' in 
    let states = map(\q -> First q) mstates in 
    let states2= map(\q -> Second q) m'states in 
    let symbols = msymbols in 
    let symbols2 = m'symbols in 
    let start = map(\q -> First q) mstart in 
    let start2 = map (\q ->Second q) m'start in 
    let finish = map(\q -> First q) mfinish in 
    let finish2 = map (\q -> Second q) m'finish in 
    let delta = map (\(q1, x, q2) -> (First q1, x, First q2)) mdelta in
    let delta2 = map (\(q1, x, q2) -> (Second q1, x, Second q2)) m'delta in
    let delta3 = liftA2(\q1 -> \q2 -> (First q1, Nothing, Second q2)) mfinish m'start in 
    (states ++ states2, nub (symbols ++ symbols2), start, finish2, (delta ++ delta2 ++ delta3)) 


starFSA :: EpsAutomaton st sy -> EpsAutomaton (Either Int st) sy
starFSA m = let (mstates, msymbols, mstart, mfinish, mdelta) = m in 
    let states = map(\q -> Second q) mstates in
    let init = First 999 in 
    let symbols = msymbols in 
    let start = (map(\q -> Second q) mstart)  in 
    let finish = map(\q -> Second q) mfinish in 
    let newdelta = liftA2 (\q1 -> \q2 -> (Second q1, Nothing, q2)) mfinish start in 
    let delta2 = map (\(q1, x, q2) -> (Second q1, x, Second q2)) mdelta in 
    let firstdelta = map (\q1 -> (First 999, Nothing, Second q1)) mstart in 
        (states ++ [init], symbols, start ++ [init], finish ++ [init], delta2 ++ newdelta ++ firstdelta) 

flatten :: Either Int Int -> Int 
flatten num = case num of
    First x -> x * 3
    Second x -> x * 2

mapStates :: (a -> b) -> EpsAutomaton a sy -> EpsAutomaton b sy
mapStates func m = let (mstates, msymbols, mstart, mfinish, mdelta) = m in 
    let states = map func mstates in
    let symbols = msymbols in  
    let start = map func mstart in 
    let finish = map func mfinish in 
    let delta = (map (\(q1, x, q2) -> ((func q1), x, (func q2))) mdelta) in 
        (states, symbols, start, finish, delta)

reToFSA :: (Eq sy) => RegExp sy -> EpsAutomaton Int sy
reToFSA regx = case regx of 
    ZeroRE -> ([2], [], [2], [], [])
    OneRE -> ([2], [], [2], [2], [])
    Lit x -> ([2, 3], [x], [2],  [3], [(2, Just x, 3)])
    Alt r1 r2 -> mapStates flatten (unionFSAs (reToFSA r1) (reToFSA r2))
    Concat r1 r2 -> mapStates flatten (concatFSAs (reToFSA r1) (reToFSA r2))
    Star r -> mapStates flatten (starFSA (reToFSA r))

