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

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

base :: Integer
base = 10

modulus :: Integer
modulus = 3

target :: Integer
target = 0

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

-- Nice, simple example.
temp = generateStateMachine 10 3 0                                

main = putStrLn $ show temp

