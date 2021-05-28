{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}

module TreeGrammars where

-- Corresponds to the definition in (5) on the handout
data Tree sy = Node sy [Tree sy] deriving Show

-- Corresponds to the tree in (7) on the handout
t7 :: Tree Char
t7 = Node 'b' [Node 'c' [Node 'a' []], Node 'a' [Node 'b' [], Node 'b' []]]

-- Corresponds to the definition in (10) on the handout
type Automaton st sy = ([st], [sy], [st], [([st],sy,st)])

--------------------------------------------------------------------------------
-- The example from section 2.4.1 on the handout

t14 :: Tree Char
t14 = Node 'a' [Node 'b' [Node 'b' [Node 'a' []], Node 'a' [Node 'b' [], Node 'a' []]]]

data Parity = Even | Odd deriving (Show,Eq)

fsta_even :: Automaton Parity Char
fsta_even = ([Even,Odd], ['a','b'], [Even],
             [ ([Even,Even], 'a', Odd),     ([Even,Even], 'b', Even), 
               ([Even,Odd],  'a', Even),    ([Even,Odd],  'b', Odd), 
               ([Odd,Even],  'a', Even),    ([Odd,Even],  'b', Odd), 
               ([Odd,Odd],   'a', Odd),     ([Odd,Odd],   'b', Even), 
               ([Even],      'a', Odd),     ([Even],      'b', Even), 
               ([Odd],       'a', Even),    ([Odd],       'b', Odd), 
               ([],          'a', Odd),     ([],          'b', Even)
             ])

--------------------------------------------------------------------------------
-- The example from section 2.4.3 on the handout

t20 :: Tree String
t20 =
    Node "*" [
        Node "*" [
            Node "that" [], 
            Node "*" [ Node "nobody" [], Node "*" [Node "met" [], Node "anybody" []] ]
        ] ,
        Node "*" [Node "surprised" [], Node "John" [] ]
    ]

-- These three states are called "NEG", "LIC" and "0" respectively on the handout (sorry)
data NegStatus = Neg | LicNeg | NegOK deriving (Show,Eq)

fsta_npi :: Automaton NegStatus String
fsta_npi = let npis = ["anybody", "ever"] in
           let licensors = ["nobody", "not"] in
           let otherwords = ["that", "met", "surprised", "John"] in
           ([NegOK, LicNeg, Neg],
            ["*"] ++ npis ++ licensors ++ otherwords,
            [NegOK, LicNeg],
            [([Neg,    Neg],    "*", Neg), 
             ([NegOK,  Neg],    "*", Neg), 
             ([Neg,    NegOK],  "*", Neg), 
             ([NegOK,  NegOK],  "*", NegOK), 
             ([LicNeg, Neg],    "*", NegOK), 
             ([LicNeg, NegOK],  "*", NegOK), 
             ([NegOK,  LicNeg], "*", NegOK), 
             ([LicNeg, LicNeg], "*", NegOK)
            ] ++ map (\s -> ([], s, NegOK)) otherwords 
              ++ map (\s -> ([], s, LicNeg)) licensors
              ++ map (\s -> ([], s, Neg)) npis
           )


