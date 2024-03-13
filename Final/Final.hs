module Final where

-- Imports everything from the FinalUltilities module. 
import FinalUtilities
import Control.Applicative(liftA, liftA2, liftA3)
import Data.List (nub)

type State = Int

---for you to use in Section 4
plainWords = ["John","Mary","ate","bought","an","apple","books","yesterday","C","laughed","because"]
whWords = ["who","what","why"]
qWords = ["Q"]
------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.



-- Please submit your answer of Section 3 and Section 4.2 as a pdf.




----------Section 1
--1A
addToFront :: a -> SnocList a -> SnocList a
addToFront x ESL = ESL ::: x
addToFront x (sl ::: y) = (addToFront x sl) ::: y

--1B
toSnoc :: [a] -> SnocList a
toSnoc xs = toSnoc' xs ESL
  where
    toSnoc' [] sl = sl
    toSnoc' (x:xs) sl = toSnoc' xs (sl ::: x)
    

--1C

--helper function
reachableStates :: Eq a => Automaton State a -> SnocList a -> [State] -> [State]
reachableStates m w startStates =
  case w of
    ESL            -> startStates
    rest ::: x     -> let nextStates = concatMap (transitions m x) startStates
                      in reachableStates m rest nextStates


-- Assuming the type of `transitions` is something like:
-- transitions :: Automaton State a -> [(State, a, State)]
getTransitions :: Automaton st sy -> [(st, sy, st)]
getTransitions (_, _, _, _, transitions) = transitions

transitions :: Eq sy => Eq st => Automaton st sy -> sy -> st -> [st]
transitions m a q = [q2 | (q1, a1, q2) <- getTransitions m, a1 == a, q1 == q]





-- Define the forward function
forward :: (Eq a) => Automaton State a -> SnocList a -> State -> Bool
forward m w q = q `elem` reachableStates m w initialState
  where
    (_, _, initialState, _, _) = m





--1D

generatesViaForward :: (Eq a) => Automaton State a -> SnocList a -> Bool
generatesViaForward automaton word = gen_or [forward automaton word q | q <- starts]
  where (_, _, starts, _, _) = automaton



--1E
forwardGeneric :: (Semiring v) => GenericAutomaton st sy v -> SnocList sy -> st -> v
forwardGeneric = undefined

--1F
fViaForward :: (Semiring v) => GenericAutomaton st sy v -> SnocList sy -> v
fViaForward = undefined

----------Section 2
--2.1A
generatesSLG :: (Eq sy) => SLG sy -> [sy] -> Bool
generatesSLG (alphabet, starts, finals, bigrams) input =
  let backwardSLG [] f = f `elem` finals
      backwardSLG (x:xs) f = any (\(prev, current) -> x == current && backwardSLG xs prev) bigrams
  in case input of
    [] -> False
    (x:xs) -> x `elem` starts && backwardSLG xs x

--2.2A
slgStress::SLG SyllableTypes 
slgStress = ([Stressed, Unstressed], [Stressed], [Unstressed], [(Stressed, Stressed), (Stressed, Unstressed), (Unstressed, Unstressed)])

--2.2B


fsaHarmony :: Automaton State SegmentPKIU
fsaHarmony = ([1,2,3], [I, P, K, MB, U], [1], [1,2,3],
       [(1, I, 2),
       (1, P, 1),
       (1, K, 1),
       (1, U, 3),
       (1, MB, 1),
       (2, I, 2),
       (2, P, 2),
       (2, K, 2),
       (2, MB, 1),
       (3, U, 3),
       (3, P, 3),
       (3, K, 3),
       (3, MB, 1)])

--2.3A
slgToFSA :: (Eq sy) => SLG sy -> Automaton (ConstructedState sy) sy
slgToFSA (alphabet, starts, finals, bigrams) =
    let states = ExtraState : [StateForSymbol s | s <- alphabet]
        initial = ExtraState
        final = [StateForSymbol s | s <- finals]
        transitions = [(ExtraState, s, StateForSymbol s) | s <- starts]
                     ++ [(StateForSymbol x, y, StateForSymbol z) | (x, y) <- bigrams, z <- alphabet]
    in (states, alphabet, [initial], final, transitions)


--------Section 4
--4.1A
data FState = QNode | WHNode | PlainNode deriving (Eq, Show)

fsta_Island :: TreeAutomaton FState String
fsta_Island = (states, symbols, initialStates, transitions)
  where
    states = [QNode, WHNode, PlainNode]
    symbols = plainWords ++ whWords ++ qWords ++ ["*","**"]
    initialStates = [PlainNode]
    transitions =
      [ ([], s, PlainNode) | s <- plainWords ] ++ 
      [ ([], s, WHNode) | s <- whWords ] ++
      [ ([], s, QNode) | s <- qWords ] ++
      [ ([PlainNode, PlainNode], "*", PlainNode) ] ++
      [ ([PlainNode, WHNode], "*", WHNode) ] ++
      [ ([PlainNode, QNode], "*", QNode) ] ++
      [ ([WHNode, WHNode], "*", WHNode) ] ++
      [ ([WHNode, QNode], "*", PlainNode) ] ++
      [ ([QNode, WHNode], "*", PlainNode) ] ++
      [ ([QNode, QNode], "*", QNode) ]


