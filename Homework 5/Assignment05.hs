{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}

module Assignment05 where

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


----- 1. 

-----1A; please put your answer to question 1A as a comment down here. 

--The function provided takes a Tree input and returns a list of elements. Given a tree, the function recursively returns the nodes of the tree. It returns these nodes using a depth-first approach.
 

-----1B & 1C
total :: (Eq a) => a -> Tree a -> Int
total symbol (Node x daughters) =
  let countFromDaughters = sum (map (total symbol) daughters) in
  if x == symbol then 1 + countFromDaughters else countFromDaughters


leftmost :: Tree a -> [a]
leftmost (Node x daughters) =
  if null daughters
    then [x]
    else x : leftmost (head daughters)



----2
allLists :: Int -> [a] -> [[a]]
allLists 0 _ = [[]]  
allLists n xs = [x:rest | x <- xs, rest <- allLists (n-1) xs]

under :: (Eq st, Eq sy) => TreeAutomaton st sy -> Tree sy -> st -> Bool
under (_, _, _, transitions) (Node x []) q = 
    any (\(fromStates, symbol, toState) -> fromStates == [q] && symbol == x && toState == q) transitions
under automaton@(states, _, _, transitions) (Node x subtrees) q =
    let checkSubtrees = and [under automaton subtree q | subtree <- subtrees]
    in any (\(fromStates, symbol, toState) -> fromStates == [q] && symbol == x && toState == q) transitions || checkSubtrees





generatesFSTA :: (Eq st, Eq sy) => TreeAutomaton st sy -> Tree sy -> Bool
generatesFSTA automaton@(states, _, finalStates, _) tree =
    any (\q -> under automaton tree q && q `elem` finalStates) states



----3 
---A1


data WHState = QNode | WHNode | PlainNode deriving (Eq, Show)

fsta_wh :: TreeAutomaton WHState String
fsta_wh = (states, symbols, initialStates, transitions)
  where
    states = [QNode, WHNode, PlainNode]
    symbols = plainWords ++ whWords ++ qWords ++ ["*"]
    initialStates = [PlainNode]
    transitions =
      [ ([PlainNode], s, PlainNode) | s <- plainWords ] ++
      [ ([WHNode], s, PlainNode) | s <- plainWords ] ++
      [ ([QNode], s, PlainNode) | s <- plainWords ] ++
      [ ([WHNode], s, WHNode) | s <- whWords ] ++
      [ ([QNode], s, QNode) | s <- qWords ] ++
      [ ([PlainNode, PlainNode], "*", PlainNode) ] ++
      [ ([PlainNode, WHNode], "*", PlainNode) ] ++
      [ ([PlainNode, QNode], "*", PlainNode) ] ++
      [ ([WHNode, WHNode], "*", WHNode) ] ++
      [ ([WHNode, QNode], "*", WHNode) ] ++
      [ ([QNode, WHNode], "*", WHNode) ] ++
      [ ([QNode, QNode], "*", QNode) ] ++
      [ ([WHNode], "*", PlainNode) | null whWords ] ++
      [ ([QNode], "*", PlainNode) | null qWords ] ++
      [ ([PlainNode], "*", WHNode) | not (null whWords) ] ++
      [ ([PlainNode], "*", QNode) | not (null qWords)] 









