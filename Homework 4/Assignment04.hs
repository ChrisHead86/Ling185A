{-# LANGUAGE FlexibleInstances #-}

module Assignment04 where

import Control.Applicative(liftA, liftA2, liftA3)

import SemiringFSA

data Numb = Z | S Numb deriving Show

distrib_lhs :: (Semiring v) => v -> v -> v -> v
distrib_lhs x y z = x &&& (y ||| z)

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.


-- 1. Semiring-based FSAs

backward :: Semiring v => GenericAutomaton st sy v -> [sy] -> st -> v
backward (_, _, _, f, _) [] q = f q  -- Base case: empty string
backward automaton (x:xs) q =
    let (_, _, _, _, delta) = automaton
        states = statesOf automaton  -- Extract the list of states
    in gen_or [delta (q, x, q1) &&& backward automaton xs q1 | q1 <- states]

-- Helper function to extract the list of states from a GenericAutomaton
statesOf :: GenericAutomaton st sy v -> [st]
statesOf (states, _, _, _, _) = states

f :: (Semiring v) => GenericAutomaton st sy v -> [sy] -> v
f automaton w =
    let (_, _, i, _, _) = automaton
    in gen_or [i q &&& backward automaton w q | q <- statesOf automaton]

-- 2. Adding the cost semiring

addCosts :: Cost -> Cost -> Cost
addCosts Inf _ = Inf  
addCosts _ Inf = Inf  
addCosts (TheInt x) (TheInt y) = TheInt (x + y)

minCost :: Cost -> Cost -> Cost
minCost Inf y = y  
minCost x Inf = x  
minCost (TheInt x) (TheInt y) = TheInt (min x y)  

instance Semiring Cost where
    x &&& y = addCosts x y  
    x ||| y = minCost x y  
    gtrue = TheInt 0  
    gfalse = Inf  




-- 3. Adding the set-of-strings semiring

--helper functions to implement instanceSemiring

cartesianProduct :: [[a]] -> [[a]] -> [[a]]
cartesianProduct [] _ = []
cartesianProduct _ [] = []
cartesianProduct xs ys = [x ++ y | x <- xs, y <- ys]

unionSets :: [[a]] -> [[a]] -> [[a]]
unionSets xs ys = xs ++ ys

instance Semiring [[a]] where
    x &&& y = cartesianProduct x y  
    x ||| y = unionSets x y  
    gtrue = [[]]  
    gfalse = []  



gfsa34 :: GenericAutomaton Int Char [[Char]]
gfsa34 = makeGFSA [] ([1,2,3], ['C','V'],
                       [(1, [""])], [(1, [""])], 
                       [((1,'V',1), ["V"]),
                        ((1,'C',2), ["C"]),
                        ((1,'V',3), ["V"]),
                        ((2,'V',1), ["V","VV"]),
                        ((2,'V',3), ["V","VV"]),
                        ((3,'C',1), [""])])

gfsa_flap :: GenericAutomaton Int Char [[Char]]
gfsa_flap = makeGFSA [] ([1,2,3], ['a','n','t'],
                       [(1, [""])], [(1, [""]), (2, [""]), (3, ["t"])], 
                       [((1,'n',1), ["n"]),
                        ((1,'t',1), ["t"]),
                        ((1,'a',2), ["a"]),
                        ((2,'a',2), ["a"]),
                        ((2,'t',3), [""]),
                        ((3,'n',1), ["tn"]),
                        ((3,'t',1), ["tt"]),
                        ((3,'a',2), ["ta","Ta"])])

--The FSA implements the following rule: The FSA returns the input string, however if a 't' is found --directly between two 'a's (i.e. "ata"), the FSA also returns the same word, but with a 'T' replacing the 't'

-- 4. Just when you thought it couldn’t get any more amazing…

gfsa5_count :: GenericAutomaton Int Char Double
gfsa5_count = makeGFSA 0.0 ([1,2,3], ['C','V'],
                             [(1, 1.0)], [(1, 1.0)], 
                             [((1,'V',1), 1.0),
                              ((1,'C',2), 1.0),
                              ((1,'V',3), 1.0),
                              ((2,'V',1), 1.0),
                              ((2,'V',3), 1.0),
                              ((3,'C',1), 1.0)])

-- the following question is optional
gfsa5_paths :: GenericAutomaton Int Char [[Int]]
gfsa5_paths = makeGFSA [] ([1,2,3], ['C','V'],
                           [(1, [[]])], [(1, [[]])], 
                           [((1,'V',1), [[1]]),
                            ((1,'C',2), [[1]]),
                            ((1,'V',3), [[1]]),
                            ((2,'V',1), [[2]]),
                            ((2,'V',3), [[2]]),
                            ((3,'C',1), [[3]])])
