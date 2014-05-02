------------------------------------------------------------------------
--
-- MultiRegExp.hs:
--
-- Simon Frankau, 2014
--
-- Program to calculate a regexp that accepts all numbers which are
-- n mod m.
--
-- We first construct a state machine for tracking the current value
-- mod m as we read digits, and then convert the state machine to an
-- RE using Brzozowski's algebraic method
--
-- See e.g. http://neumannhaus.com/christoph/papers/2005-03-16.DFA_to_RegEx.pdf
--
-- This is a generalisation of FIXME.
--

{-# LANGUAGE TupleSections #-}

module Main where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Map ((!))
import Data.Maybe (mapMaybe)

import System.Environment (getArgs)

------------------------------------------------------------------------
-- State machine generation
--

type State = String
type Symbol = Char

-- Transitions is a map from initial state to a map from end state
-- to the label of the edge that connects them. Yes, it's a bit odd
-- but it makes our later work easier!
data FSM a = FSM {
    initState   :: State,
    endState    :: State,
    transitions :: Map.Map State (Map.Map State a)
} deriving (Eq, Ord)

instance Show a => Show (FSM a) where
    show x =
        "Init: " ++ initState x ++ "\n" ++
        "End: "  ++ endState  x ++ "\n" ++
        showTransitions (transitions x)

showTransitions :: Show a => Map.Map State (Map.Map State a) -> String
showTransitions trans =
    List.intercalate "\n" $
        map (\(x, (y, z)) -> x ++ " -" ++ show z ++ "-> " ++ y) $
        concatMap (\(x, ys) -> map (x,) $ Map.toAscList ys) $
        Map.toAscList trans

-- We will name the states after the actual numbers modulo whatever.
generateStateMachine :: Integer -> Integer -> Integer -> FSM (Set.Set Integer)
generateStateMachine base modulus target =
    FSM {
        initState   = show 0,
        endState    = show target,
        transitions = Map.fromList $ map buildTrans [0..modulus-1]
    } where
        buildTrans n = (show n,
                        Map.fromListWith Set.union $
                            map (buildEdge n) [0..base-1])
        buildEdge n b = (show $ (n * base + b) `mod` modulus,
                         Set.singleton b)

------------------------------------------------------------------------
-- Generate regexps

data RegExp = Terminal Symbol
            | Concat [RegExp]
            | Alt (Set.Set RegExp)
            | Close RegExp
              deriving (Eq, Ord)

instance Show RegExp where
    show (Terminal x) = x:[]
    show (Concat xs)  = "(" ++ concatMap show xs ++ ")"
    show (Alt xs)     = buildAltStr (getTerms xs) (getNonTerms xs)
    show (Close x)    = show x ++ "*"

getTerms :: Set.Set RegExp -> [Symbol]
getTerms = mapMaybe getTerm . Set.toList where
    getTerm (Terminal x) = Just x
    getTerm _            = Nothing

getNonTerms :: Set.Set RegExp -> [RegExp]
getNonTerms = mapMaybe getNonTerm . Set.toList where
    getNonTerm (Terminal _) = Nothing
    getNonTerm x            = Just x

buildAltStr :: [Symbol] -> [RegExp] -> String
buildAltStr terms nonterms =
    case (terms', nonterms') of
        (Just t,  Nothing) -> t
        (Nothing, Just nt) -> "(" ++ nt ++ ")"
        (Just t,  Just nt) -> "(" ++ t ++ "|" ++ nt ++ ")"
    where
            terms' = case terms of
                []    -> Nothing
                x@[_] -> Just x
                xs    -> Just $ "[" ++ xs ++ "]"
            nonterms' = case nonterms of
                [] -> Nothing
                xs -> Just $ List.intercalate "|" $ map show xs

isTerminal :: RegExp -> Bool
isTerminal (Terminal _) = True
isTerminal _            = False

-- A few construction operators that simplify as we go...

reConcat :: RegExp -> RegExp -> RegExp
reConcat x y = conc (unCat x ++ unCat y) where
    unCat (Concat xs) = xs
    unCat x = [x]
    conc [x] = x
    conc xs  = Concat xs

reAlt :: RegExp -> RegExp -> RegExp
reAlt x y = Alt (unAlt x `Set.union` unAlt y) where
    unAlt (Alt xs) = xs
    unAlt x = Set.singleton x

reClose :: RegExp -> RegExp
reClose x@(Close _) = x
reClose x = Close x

reNothing :: RegExp
reNothing = Alt Set.empty

reEverything :: RegExp
reEverything = Concat []

-- Get an appropriate character from a number
getSym :: Integer -> Symbol
getSym i = Char.chr $ symBase i + fromIntegral i where
  symBase i | i < 10    = Char.ord '0'
  symBase i | otherwise = Char.ord 'A' - 10

-- Convert our finite state machine to one where the edges are REs.
convertStateMachine :: FSM (Set.Set Integer) -> FSM RegExp
convertStateMachine fsm =
    fsm { transitions = Map.map convertTrans $ transitions fsm } where
          convertTrans = Map.map (Alt . Set.map makeTerm)
          makeTerm = Terminal . getSym

-- Add an explicit terminal state, which simplifies later work.
addFinalState :: FSM RegExp -> FSM RegExp
addFinalState fsm =
    fsm { endState = end,
          transitions = Map.adjust addFinal (endState fsm) (transitions fsm) }
        where
            end = "<END>"
            addFinal = Map.insert end reEverything

-- The regular expression is generated by repeated substitution and
-- solving of the states.

-- First we implement solving:
--
-- NB: We distribute the concenation of the X* bit over the
-- alternation which is dumb but at least simple.
--
-- i.e. we do X = AX + BY + CZ -> X = A*BY + A*CZ, rather than A*(BY + CZ)
solveState :: State -> FSM RegExp -> FSM RegExp
solveState state fsm =
    fsm { transitions = Map.adjust solve state $ transitions fsm } where
        solve edges = case Map.lookup state edges of
            Just x  -> Map.map (reConcat (reClose x)) $ Map.delete state edges
            Nothing -> edges

-- Then we implement substitution:
substState :: State -> State -> FSM RegExp -> FSM RegExp
substState src tgt fsm =
    fsm { transitions = Map.adjust subst tgt $ transitions fsm } where
        srcEdges = transitions fsm ! src
        subst tgtEdges = Map.unionWith reAlt tgtEdges' srcEdges' where
            tgtEdges' = Map.delete src tgtEdges
            srcEdges' = case Map.lookup src tgtEdges of
                Nothing -> Map.empty
                Just re -> Map.map (reConcat re) srcEdges

-- Remove a state that is no longer needed (no checks):
deleteState :: State -> FSM RegExp -> FSM RegExp
deleteState state fsm =
    fsm { transitions = Map.delete state $ transitions fsm }

-- Helper to get list of states
getStates :: FSM RegExp -> [State]
getStates = Map.keys . transitions

-- Eliminate a state: Solve it, substitute it into all other states,
-- and delete it
elimState :: State -> FSM RegExp -> FSM RegExp
elimState state fsm =
   deleteState state $ substAllStates $ solveState state fsm where
       substAllStates fsm = List.foldl' (flip $ substState state) fsm states
       states = getStates fsm

-- Extract the final result
extractResult :: FSM RegExp -> RegExp
extractResult fsm = transitions fsm ! initState fsm ! endState fsm

-- Now, a very dumb solver for the entire FSM
solveFSM :: FSM RegExp -> RegExp
solveFSM fsm =
    extractResult $
    solveState (initState fsm) $
    List.foldl' (flip elimState) fsm statesToElim where
        statesToElim = List.delete (initState fsm) $ getStates fsm

-- Glue it all together
genRE :: Integer -> Integer -> Integer -> String
genRE base modulus target =
    show $ solveFSM $ addFinalState $ convertStateMachine $
        generateStateMachine base modulus target

-- Nice usage message
usage :: String
usage = List.intercalate "\n" [
    "Usage:",
    "    MultiRegExp base modulus target",
    "Prints a regexp which (only) matches all numbers (represented in ",
    "base 'base') that are equal to 'target', modulo 'modulus'."]

-- Entry point (duh :)
main = do
    args <- getArgs
    putStrLn $ case map read args of
        [base, modulus, target] -> genRE base modulus target
        _ -> usage
