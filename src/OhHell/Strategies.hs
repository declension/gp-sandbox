module OhHell.Strategies where

import Prelude ()
import ClassyPrelude hiding (fromList)

import OhHell.Core
import OhHell.Player
import OhHell.Rules
import qualified Data.List.NonEmpty as NonEmpty

import Control.Monad.Random (MonadRandom,getRandomR)
import qualified Data.Set as Set
import qualified Data.List as List


data RandomBidder = RandomBidder PlayerId deriving (Show, Eq, Ord)

instance Player RandomBidder where
  getPlayerId (RandomBidder pid) = pid
  getPlayerName (RandomBidder pid) = pid

  chooseBid player dealerRules trumps bidsSoFar (Hand cards) = do
    let cardsThisRound = NonEmpty.length cards
    let options = Set.toList $ validBids dealerRules cardsThisRound bidsSoFar
    rnd <- getRandomR (0, List.length options - 1)
    return $ options List.!! rnd

  chooseCard player dealerRules bids played (Hand hand) = do
    rnd <- getRandomR (0, NonEmpty.length hand - 1)
    return $ hand NonEmpty.!! rnd
