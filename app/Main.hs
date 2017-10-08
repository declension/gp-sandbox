{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import GP (myFitness, eval)
import GenProg
import Control.Monad.Random (mkStdGen, evalRand, getStdGen)
import Text.Printf (printf)

import Data.List.NonEmpty as NonEmpty (fromList)

import OhHell
import Control.Monad.State (StateT, execStateT)
import ClassyPrelude hiding (last)
import Data.List (last)

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
  let bob = Player 2 "Bob"
  let charlie = Player 3 "Charlie"
  let prr = PlayerRoundResult
  return ()
  let results = NonEmpty.fromList [(alice,   [prr 0 0, prr 1 0, prr 1 1, prr 3 3]),
                                   (bob,     [prr 0 1, prr 0 1, prr 1 0, prr 0 0]),
                                   (charlie, [prr 0 1, prr 0 0, prr 1 1, prr 0 1])]
  let scoringRules = ProgressiveScoring 10 (-1)
  print $ scoresFor scoringRules results
  finalResults <- execStateT (playRound scoringRules) results
  print finalResults
