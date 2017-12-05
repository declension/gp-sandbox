module OhHell.Player where

import Prelude ()
import ClassyPrelude

import OhHell.Core
import OhHell.Pretty
import OhHell.Rules
import Control.Monad.Random (MonadRandom)
import Game.Implement.Card.Standard (Suit,PlayingCard)
import Data.List.NonEmpty (NonEmpty)

-- | The player typeclass
class (Show p, Eq p, Ord p, Pretty p) => Player p where
  -- | Return the player ID
  getPlayerId :: p -> PlayerId

  -- | Return the player name (may be the ID)
  getPlayerName :: p -> String

  -- | Select a bid given some input
  chooseBid :: (DealerRules d, MonadRandom m)
             => p
             -> d               -- ^ Rules of the game
             -> Maybe Suit      -- ^ Trumps if any
             -> BidsFor p       -- ^ What has been bid so far
             -> Hand            -- ^ The hand this round on which to bid
             -> m Bid           -- ^ Resulting bid

  -- | Select a card to play given some input
  chooseCard :: (DealerRules d, MonadRandom m)
             => p
             -> d               -- ^ Rules of the game
             -> Maybe Suit      -- ^ Trumps if any
             -> BidsFor p       -- ^ What has been bid
             -> Hand            -- ^ (What remains of) the player's hand
             -> Trick p         -- ^ What has been played so far this trick
             -> m PlayingCard   -- ^ The chosen card



