{-# LANGUAGE NoImplicitPrelude #-}
module OhHell.Strategy where
import ClassyPrelude hiding (fromList)
import qualified Data.List.NonEmpty as NonEmpty

import OhHell
import Control.Monad.Random (MonadRandom,getRandomR)
import qualified Data.Set as Set
import qualified Data.List as List

class (Show b) => BidStrategy b where
  -- | All players bid for a round
  calculateBid :: (DealerRules d, MonadRandom m)
             => b
             -> d
             -> Player
             -> PlayerBids
             -> Hand
             -> m Bid

data RandomBidder = RandomBidder deriving Show

instance BidStrategy RandomBidder where
  calculateBid bs dealerRules player bidsSoFar (Hand cards) = do
    let cardsThisRound = NonEmpty.length cards
    let options = Set.toList $ validBids dealerRules cardsThisRound bidsSoFar
    rnd <- getRandomR (0, len options - 1)
    return $ options List.!! rnd
