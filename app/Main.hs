module Main where

import Prelude (putStrLn)
import ClassyPrelude hiding (last, putStrLn)

import OhHell.Rules
import OhHell.Pretty
import OhHell.Strategies
import OhHell.Game (shuffledDeck, runGame)
import OhHell.Player (Player, AnyPlayer, fromPlayer)

import GP (myFitness, eval)
import GenProg
import Control.Monad.Random (mkStdGen, evalRand, getStdGen)
import Text.Printf (printf)
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty)
import Data.List (last)
import Control.Monad.Writer (WriterT, runWriterT,execWriterT)
import Game.Implement.Card (fullDeck)
import Game.Implement.Card.Standard (PlayingCard)


-- | Evolve a test function
evolveMain :: IO ()
evolveMain = do
  let target = 12345 :: Int
      params = defaultEvolParams {fitness = myFitness target}
  printf "Evolving expressions that result in something near %d...\n" target
  rng <- getStdGen
  let something = evalRand (evolveTrace params {elitists = 1, mProb=0.1, miProb = 0.2}) rng
      meanwhile = map (sFitness . best . pop) something
      result = cachedBest $ last something
      prettyExpr = show (unInd result)
  printf "Progress was %s\n" $ intercalate ", " $ map show meanwhile
  printf "Evaluates to: %s using %s.\n" (maybe "??" show (eval $ unInd result)) prettyExpr

players :: NonEmpty AnyPlayer
players = NonEmpty.fromList [alice, bob, charlie, dave]
    where alice   = fromPlayer $ LessRandomPlayer "Alice"
          bob     = fromPlayer $ RandomPlayer "Bob"
          charlie = fromPlayer $ LessRandomPlayer "Charlie"
          dave    = fromPlayer $ RandomPlayer "Dave"


main :: IO ()
main = do
  let scoringRules = ProgressiveScoring 10 (-1)
  let dealingRules = RikikiDealingFor (NonEmpty.length players)
  deck <- shuffledDeck
  (log, finalResults) <- runGame dealingRules scoringRules players deck
  putStrLn log
  putStrLn $ "Scores are:" ++ prettify (scoresFor scoringRules finalResults)
