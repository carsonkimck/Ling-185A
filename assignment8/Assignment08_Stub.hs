{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}

module Assignment08 where

import Data.List

import Control.Applicative(liftA, liftA2, liftA3)

import TreeGrammars

plainWords = ["John","Mary","ate","bought","an","apple","books","yesterday","C","laughed","because"]
whWords = ["who","what","why"]
qWords = ["Q"]

------------------------------------------------------
-- Some tiny helpers for writing trees more compactly

lf :: a -> Tree a
lf x = Node x []

mrg :: Tree String -> Tree String -> Tree String
mrg t1 t2 = Node "*" [t1,t2]

------------------------------------------------------

-- (1a)/(2a) `C John ate an apple yesterday'
tree_1a :: Tree String
tree_1a = mrg (lf "C") (mrg (lf "John") (mrg (mrg (lf "ate") (mrg (lf "an") (lf "apple"))) (lf "yesterday")))

-- (1b)/(2b) `Q John ate what yesterday'
tree_1b :: Tree String
tree_1b = mrg (lf "Q") (mrg (lf "John") (mrg (mrg (lf "ate") (lf "what")) (lf "yesterday")))

-- (3a) `Q John ate an apple yesterday'
tree_3a :: Tree String
tree_3a = mrg (lf "Q") (mrg (lf "John") (mrg (mrg (lf "ate") (mrg (lf "an") (lf "apple"))) (lf "yesterday")))

-- (3b) `C John ate what yesterday'
tree_3b :: Tree String
tree_3b = mrg (lf "C") (mrg (lf "John") (mrg (mrg (lf "ate") (lf "what")) (lf "yesterday")))

tree_13 :: Tree String
tree_13 =
    Node "*" [
        Node "Q" [],
        Node "*" [
            Node "John" [],
            Node "*" [
                Node "laughed" [],
                Node "**" [
                    Node "because" [],
                    Node "*" [
                        Node "Mary" [],
                        Node "*" [
                            Node "*" [Node "bought" [], Node "books" []],
                            Node "why" []
                        ]
                    ]
                ]
            ]
        ]
    ]

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.

--- 1 Warm-ups: Working with trees ---

count :: (Eq a) => a -> Tree a -> Int
count a tree = case tree of 
    Node sym [] -> if sym == a then 1 else 0
    Node sym lst -> if sym == a then 1 + sum ((map (count a) lst)) else 0 + sum (map (count a) lst)

leftEdge :: Tree a -> [a]
leftEdge tree = case tree of 
    Node sym [] -> [sym]
    Node sym (x:rest) -> [sym] ++ (leftEdge x)
    
----- 2 Computing “under” values ---- 

allLists :: Int -> [a] -> [[a]]
allLists num list = case num of 
    0 -> [[]]
    _ -> liftA2 (\x -> \y -> x:y) list (allLists (num-1) list) 

under :: (Eq st, Eq sy) => Automaton st sy -> Tree sy -> st -> Bool
under m tree state = let (states, syms, i, delta) = m in case tree of 
   Node x [] -> elem ([], x, state) delta 
   Node x list -> let allStates = allLists (length list) states in 
        let underHelper st = and ((map(\(t, s) -> under m t s) (zip list st)))
       in or (map (\q -> elem (q, x, state) delta && underHelper q) allStates)

generates :: (Eq st, Eq sy) => Automaton st sy -> Tree sy -> Bool
generates m tree = let (states, syms, i, delta) = m in 
    or(map(\q-> elem q i && under m tree q) states)

---- 3 Wh-in-situ dependencies -------

data WhStatus = QWord | WhWord | WhOk deriving (Show,Eq)

fsta_wh1 :: Automaton WhStatus String
fsta_wh1 = ([QWord, WhWord, WhOk], 
            plainWords ++ whWords ++ qWords ++ ["*"],
            [WhOk], 
            [([WhWord, WhWord], "*", WhWord),
            ([WhOk, WhWord], "*", WhWord),
            ([WhWord, WhOk], "*", WhWord),
            ([WhOk, WhOk], "*", WhOk),
            ([QWord, WhWord], "*", WhOk),
            ([WhWord, QWord], "*", WhOk)  
            ] ++ map (\s -> ([], s, QWord)) qWords
              ++ map (\s -> ([], s, WhWord)) whWords
              ++ map (\s -> ([], s, WhOk)) plainWords
        
            )


fsta_wh2 :: Automaton WhStatus String
fsta_wh2 = ([QWord, WhWord, WhOk], 
            plainWords ++ whWords ++ qWords ++ ["*", "**"],
            [WhOk], 
            [([WhWord, WhWord], "*", WhWord),
            ([WhOk, WhWord], "*", WhWord),
            ([WhWord, WhOk], "*", WhWord),
            ([WhOk, WhOk], "*", WhOk),
            ([QWord, WhWord], "*", WhOk),
            ([WhWord, QWord], "*", WhOk),   
            ([WhOk, WhOk], "**", WhOk),
            ([QWord, WhWord], "**", WhOk),
            ([WhWord, QWord], "**", WhOk)
            ] ++ map (\s -> ([], s, QWord)) qWords
              ++ map (\s -> ([], s, WhWord)) whWords
              ++ map (\s -> ([], s, WhOk)) plainWords    
            )


