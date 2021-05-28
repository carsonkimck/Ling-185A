{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}

module ContextFree where

import Control.Applicative(liftA, liftA2, liftA3)

data Cat = S | NP | VP | PP | V | P deriving (Show, Eq, Ord)

data RewriteRule nt t = NTRule nt (nt,nt) | TRule nt t deriving (Show, Eq)

-- Corresponds to the definition in (3) on the handout
type CFG nt t = ([nt], [t], [nt], [RewriteRule nt t])

-----------------------------------------------------------
-- CFGs generalized to allow non-boolean result values

type GenericCFG nt t v = ([nt], [t], nt -> v, RewriteRule nt t -> v)

-- Feel free to ignore the details of this function.
makeGCFG :: (Eq nt, Eq t) => v -> ([nt], [t], [(nt,v)], [(RewriteRule nt t, v)]) -> GenericCFG nt t v
makeGCFG def (nts, ts, starts, rules) =
    let mylookup l x = case lookup x l of {Just y -> y; Nothing -> def} in
    (nts, ts, mylookup starts, mylookup rules)

-----------------------------------------------------------
-- Familiar semiring stuff, same as last week

class Semiring a where
    (&&&) :: a -> a -> a
    (|||) :: a -> a -> a
    gtrue :: a
    gfalse :: a

gen_or :: Semiring a => [a] -> a
gen_or list = case list of {[] -> gfalse; (x:xs) -> x ||| (gen_or xs)}

gen_and :: Semiring a => [a] -> a
gen_and list = case list of {[] -> gtrue; (x:xs) -> x &&& (gen_and xs)}

instance Semiring Bool where
    x &&& y = x && y
    x ||| y = x || y
    gtrue = True
    gfalse = False

instance Semiring Double where
    x &&& y = x * y
    x ||| y = x + y
    gtrue = 1.0
    gfalse = 0.0

-----------------------------------------------------------
-- Some example grammars

-- From (14) on the handout
cfg14 :: GenericCFG Cat String Bool
cfg14 = makeGCFG False ([VP,NP,PP,V,P], ["watches","spies","telescopes","with"], 
                        [(VP,True)], 
                        [(NTRule VP (V,NP), True),   (NTRule NP (NP,PP), True),     (NTRule PP (P,NP), True),
                         (NTRule VP (VP,PP), True),  (TRule NP "telescopes", True),
                         (TRule VP "watches", True), (TRule NP "watches", True),    (TRule P "with", True), 
                         (TRule VP "spies", True),   (TRule NP "spies", True),      (TRule V "watches", True)
                        ]
                       )

-- A probabilistic version of cfg14
cfg14a :: GenericCFG Cat String Double
cfg14a = makeGCFG 0.0 ([VP,NP,PP,V,P], ["watches","spies","telescopes","with"], 
                       [(VP,1.0)], 
                       [(NTRule VP (V,NP), 0.4),   (NTRule NP (NP,PP), 0.2),     (NTRule PP (P,NP), 1.0), 
                        (NTRule VP (VP,PP), 0.3),  (TRule NP "telescopes", 0.3), 
                        (TRule VP "watches", 0.2), (TRule NP "watches", 0.3),    (TRule P "with", 1.0), 
                        (TRule VP "spies", 0.1),   (TRule NP "spies", 0.2),      (TRule V "watches", 1.0)
                       ]
                      )

-- Another probabilistic version of cfg14, with starting probability 
-- split evenly between VP and NP
cfg14b :: GenericCFG Cat String Double
cfg14b = makeGCFG 0.0 ([VP,NP,PP,V,P], ["watches","spies","telescopes","with"], 
                       [(VP,0.5), (NP,0.5)], 
                       [(NTRule VP (V,NP), 0.4),   (NTRule NP (NP,PP), 0.2),     (NTRule PP (P,NP), 1.0), 
                        (NTRule VP (VP,PP), 0.3),  (TRule NP "telescopes", 0.3), 
                        (TRule VP "watches", 0.2), (TRule NP "watches", 0.3),    (TRule P "with", 1.0), 
                        (TRule VP "spies", 0.1),   (TRule NP "spies", 0.2),      (TRule V "watches", 1.0)
                       ]
                      )

-----------------------------------------------------------
-- Functions for inside values

insideBool :: GenericCFG nt t Bool -> [t] -> nt -> Bool
insideBool cfg str n =
    let (nts, ts, i, r) = cfg in
    case str of
    [] -> False
    (x:[]) -> r (TRule n x)
    (x:y:rest2) -> let conj i ld rd = r (NTRule n (ld,rd)) && 
                                        insideBool cfg (take i str) ld && 
                                        insideBool cfg (drop i str) rd
                   in
                   or (liftA3 conj [1 .. (length str - 1)] nts nts)

inside :: (Semiring v) => GenericCFG nt t v -> [t] -> nt -> v
inside cfg str n =
    let (nts, ts, i, r) = cfg in
    case str of
    [] -> gfalse
    (x:[]) -> r (TRule n x)
    (x:y:rest2) -> let conj i ld rd = r (NTRule n (ld,rd)) &&& 
                                        inside cfg (take i str) ld &&& 
                                        inside cfg (drop i str) rd
                   in
                   gen_or (liftA3 conj [1 .. (length str - 1)] nts nts)




[[([],["He","watches","spies","with","telescopes"]),
([NP],["watches","spies","with","telescopes"]),
([NP,VP],["spies","with","telescopes"]),
([NP,VP,VP],["with","telescopes"]),
([NP,VP,VP,P],["telescopes"]),
([NP,VP,VP,P,NP],[]),
([NP,VP,VP,PP],[]),
([NP,VP],["spies","with","telescopes"]),
([NP,VP,NP],["with","telescopes"]),
([NP,VP,NP,P],["telescopes"]),
([NP,VP,NP,P,NP],[]),
([NP,VP,NP,PP],[]),
([NP,VP,NP],[]),
([NP,VP],[]),
([NP,VP,NP],["with","telescopes"]),
([NP,VP],["with","telescopes"]),
([NP,VP,P],["telescopes"]),
([NP,VP,P,NP],[]),
([NP,VP,PP],[]),
([NP,VP],[]),
([NP,VP],["with","telescopes"]),
([S],["with","telescopes"]),
([S,P],["telescopes"]),
([S,P,NP],[]),
([NP,VP],["spies","with","telescopes"]),
([S],["spies","with","telescopes"]),
([S,VP],["with","telescopes"]),
([S,VP,P],["telescopes"]),
([S,VP,P,NP],[]),
([S,VP,PP],[]),
([S],["spies","with","telescopes"]),
([S,NP],["with","telescopes"]),
([S,NP,P],["telescopes"]),
([S,NP,P,NP],[]),
([S,NP,PP],[]),
([NP],["watches","spies","with","telescopes"]),
([NP,NP],["spies","with","telescopes"]),
([NP,NP,VP],["with","telescopes"]),
([NP,NP,VP,P],["telescopes"]),
([NP,NP,VP,P,NP],[]),
([NP,NP,VP,PP],[]),
([NP,NP,VP],[]),
([NP,NP,VP],["with","telescopes"]),
([NP,S],["with","telescopes"]),
([NP,S,P],["telescopes"]),
([NP,S,P,NP],[]),
([NP,NP],["spies","with","telescopes"]),
([NP,NP,NP],["with","telescopes"]),
([NP,NP,NP,P],["telescopes"]),
([NP,NP,NP,P,NP],[]),
([NP,NP,NP,PP],[]),
([NP],["watches","spies","with","telescopes"]),
([NP,V],["spies","with","telescopes"]),
([NP,V,VP],["with","telescopes"]),
([NP,V,VP,P],["telescopes"]),
([NP,V,VP,P,NP],[]),
([NP,V,VP,PP],[]),
([NP,V],["spies","with","telescopes"]),
([NP,V,NP],["with","telescopes"]),
([NP,V,NP,P],["telescopes"]),
([NP,V,NP,P,NP],[]),
([NP,V,NP,PP],[]),
([NP,V,NP],[]),
([NP,VP],[]),
([NP,V,NP],["with","telescopes"]),
([NP,VP],["with","telescopes"]),
([NP,VP,P],["telescopes"]),
([NP,VP,P,NP],[]),
([NP,VP,PP],[]),
([NP,VP],[]),
([NP,VP],["with","telescopes"]),
([S],["with","telescopes"]),
([S,P],["telescopes"]),
([S,P,NP],[])]]


