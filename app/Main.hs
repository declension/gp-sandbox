{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import OhHell.Strategies

import GP (myFitness, eval)
import GenProg
import Control.Monad.Random (mkStdGen, evalRand, getStdGen)
import Text.Printf (printf)

import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty)

import OhHell
import OhHell.Game (playGame)
import Control.Monad.State (StateT, execStateT,evalStateT,runStateT)
import ClassyPrelude hiding (last, putStrLn)
import qualified Prelude
import Data.List (last)
import Control.Monad.Writer (WriterT, runWriterT,execWriterT)
import Game.Implement.Card (fullDeck)
import Game.Implement.Card.Standard (PlayingCard)

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

players :: NonEmpty RandomBidder
players = NonEmpty.fromList [alice, bob, charlie]
    where alice   = RandomBidder "Alice"
          bob     = RandomBidder "Bob"
          charlie = RandomBidder "Charlie"

main :: IO ()
main = do
  let scoringRules = ProgressiveScoring 10 (-1)
  let dealingRules = RikikiDealingFor (NonEmpty.length players)
  (log, finalResults) <- runStateT (execWriterT (playGame dealingRules scoringRules players)) []
  Prelude.putStrLn log
  Prelude.putStrLn $ "Scores are:" ++ show (scoresFor scoringRules finalResults)
