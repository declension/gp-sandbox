module Main where

import GP (myFitness, eval)
import GenProg
import Control.Monad.Random (mkStdGen, evalRand, getStdGen)
import Data.Maybe (fromMaybe)
import Text.Printf (printf)
import Data.List (intercalate)

import OhHell
import Control.Monad.State (StateT, execStateT)

evolveMain :: IO ()
evolveMain = do
  let target = 12345
  printf "Evolving expressions that result in something near %d...\n" target
  let params = defaultEvolParams {fitness = myFitness target}
  rng <- getStdGen
  let trace = evalRand (evolveTrace params {elitists = 1, mProb=0.1, miProb = 0.2}) rng
  let meanwhile = map (sFitness . best . pop) trace
  printf "Progress was %s\n" $ intercalate ", " $ map show meanwhile
  let result = cachedBest $ last trace
  let prettyExpr = show (unInd result)
  printf "Evaluates to: %s using %s.\n" (maybe "??" show (eval $ unInd result)) prettyExpr

type MyState = StateT GameState IO ()

main :: IO ()
main = do
  print deck
  let alice = Player 1 "Alice"
  let scores = [RoundResult [(alice, HandResult 1 2)]]
  results <- execStateT playGame scores
  print results
