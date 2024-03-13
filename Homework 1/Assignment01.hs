module Assignment01 where

-- Imports everything from the Recursion module. 
import Recursion
import FiniteState

-- Another type that gives us something vaguely linguistic to play with,
-- which we'll use as symbols for some FSAs
data SegmentPKIU = P | K | I | U | MB deriving (Show,Eq)

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


-- Please submit your answer of Part 1 and Part 5B as a pdf on BruinLearn. 

----------Section 2

-- Neg (Neg (Cnj T F))

-- Disj (Neg T) (Cnj (Neg T) (Neg (Neg T)))


depth :: Form -> Numb
depth = \x -> case x of
    T -> S Z
    F -> S Z
    Neg f -> S (depth f)
    Cnj f1 f2 -> maxDepth (depth f1) (depth f2) (S Z)
    Dsj f1 f2 -> maxDepth (depth f1) (depth f2) (S Z)

maxDepth :: Numb -> Numb -> Numb -> Numb
maxDepth = \n1 n2 n3 -> case (n1, n2, n3) of
    (Z, n2, _) -> n2
    (n1, Z, _) -> n1
    (_, _, n3) -> maxDepth (S n1) (S n2) (S n3)


countNegs :: Form -> Numb
countNegs = \x -> case x of
    T -> Z
    F -> Z
    Neg f -> S (countNegs f)
    Cnj f1 f2 -> addNumbs (countNegs f1) (countNegs f2)
    Dsj f1 f2 -> addNumbs (countNegs f1) (countNegs f2)

addNumbs :: Numb -> Numb -> Numb
addNumbs = \n1 n2 -> case (n1, n2) of
    (Z, n) -> n
    (S m, n) -> S (addNumbs m n)


----------Section 3
multiply :: Numb -> (Numb -> Numb)
multiply = \x -> case x of
    Z    -> \_ -> Z
    S Z  -> \y -> y
    S n  -> \y -> add y (multiply n y)

isEqual :: Numb -> (Numb -> Bool)
isEqual = \x -> case x of
    Z -> \y -> case y of
        Z -> True
        S _ -> False
    S n -> \y -> case y of
        Z -> False
        S m -> isEqual n m



---------Section 4

listOf :: Numb -> (a -> [a])
listOf = \n -> case n of
    Z -> const []
    S n' -> \x -> x : (listOf n' x)


addToEnd :: a -> ([a] -> [a])
addToEnd = \x -> \l -> case l of
    [] -> [x]
    y:ys -> y : (addToEnd x ys)


reverseL :: [a] -> [a]
reverseL = \l -> case l of
    [] -> []
    x:xs -> (reverseL xs) ++ [x]


adjacentVowels :: [SegmentPKIU] -> Bool
adjacentVowels = \l -> case l of
    [] -> False
    (x:y:rest) -> case (x, y) of
        (I, I) -> True
        _ -> adjacentVowels (y:rest)
    _ -> False



--------Section 5


fsa_countCs::Automaton SegmentCV
fsa_countCs = ([20,51,13,48], [C,V], [20], [48], [(20, V, 20),
						(20, C, 51),
						(51, V, 51),
						(51, C, 13),
						(13, V, 13),
						(13, C, 20),
						(13, C, 48),
						(48, V, 48)])





