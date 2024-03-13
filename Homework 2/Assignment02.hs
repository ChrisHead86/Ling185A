module Assignment02 where

import Prelude hiding (Either(..))

import Control.Applicative(liftA, liftA2, liftA3)
import Data.List(nub)

import FiniteStatePart2


---------------------------------------
-- Setup for section 3

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
--            Write all your code below this line.

-----Section 1
fsa_oddEven :: Automaton Int SegmentCV
fsa_oddEven = ([0,1,2,3], [C,V], [0], [2], [(0, C, 1),
					(0, V, 2),
					(1, C, 0),
					(1, V, 3),
					(3, V, 1),
					(3, C, 2),
					(2, V, 0),
					(2, C, 3)])


requireCs :: Int -> Automaton Int SegmentCV
requireCs n = 
    let states = [0 .. n]
        syms = [C,V]
        i = [0]
        f = [n]
        ctransitions = map (\q -> (q, C, q + 1)) [0 .. n - 1]
        vtransitions = map (\q -> (q, V, q)) [0 .. n]
    in (states, syms, i, f, ctransitions ++ vtransitions)
	



----Please submit your answer to Section 2 in a pdf




----Section 3


unionFSAs :: (Eq sy, Eq st) => EpsAutomaton st sy -> EpsAutomaton st sy -> EpsAutomaton st sy
unionFSAs (states1, syms1, i1, f1, trans1) (states2, syms2, i2, f2, trans2) = 
    let newStates = states1 ++ states2
        newSyms = syms1 ++ syms2
        i = i1 ++ i2
        f = f1 ++ f2
        newTrans = trans1 ++ trans2
    in (newStates, newSyms, i, f, newTrans)




concatFSAs :: (Eq sy, Eq st) => EpsAutomaton st sy -> EpsAutomaton st sy -> EpsAutomaton st sy
concatFSAs (states1, syms1, i1, f1, trans1) (states2, syms2, i2, f2, trans2) =
    let newStates = states1 ++ states2
        newSyms = syms1 ++ syms2
        i = i1
        f = f2 
        newTrans = trans1 ++ trans2 ++ [(fState, Nothing, initState2) | fState <- f1, initState2 <- i2]
    in (newStates, newSyms, i, f, newTrans)



starFSA :: (Eq sy, Eq st, Num st) => EpsAutomaton st sy -> EpsAutomaton st sy
starFSA (states, syms, i, f, trans) = 
    let newStates = states
        newSyms = syms
        newI = [last states + 1] 
        newF = f ++ newI
        newTrans = trans ++ [(fState, Nothing, iState) | fState <- f, iState <- i] ++ [(newI !! 0, Nothing, iState) | iState <- i]
    in (newStates ++ newI, newSyms, newI, newF, newTrans)


flatten :: Either Int Int -> Int
flatten (First x) = 2 * x
flatten (Second y) = 2 * y + 1


mapStates :: (Eq sy, Eq a, Eq b) => (a -> b) -> EpsAutomaton a sy -> EpsAutomaton b sy
mapStates f (states, syms, i, f', trans) = 
    let newStates = map f states
        newI = map f i
        newF = map f f'
        newTrans = [(f fromState, label, f toState) | (fromState, label, toState) <- trans]
    in (newStates, syms, newI, newF, newTrans)

reToFSA :: (Eq sy) => RegExp sy -> EpsAutomaton Int sy
reToFSA (Lit x) = ([0, 1], [x], [0], [1], [(0, Just x, 1)])
reToFSA (Alt r1 r2) = unionFSAs (mapStates (+2) (reToFSA r1)) (mapStates (+(2 + numStates (reToFSA r1))) (reToFSA r2))
reToFSA (Concat r1 r2) = concatFSAs (mapStates (+2) (reToFSA r1)) (mapStates (+(2 + numStates (reToFSA r1))) (reToFSA r2))
reToFSA (Star r) = starFSA (mapStates (+1) (reToFSA r))
reToFSA ZeroRE = ([0, 1], [], [0], [1], [(0, Nothing, 1)])
reToFSA OneRE = ([0, 1], [], [0, 1], [1], [(0, Nothing, 1)])

numStates :: EpsAutomaton st sy -> st
numStates (states, _, _, _, _) = last states

