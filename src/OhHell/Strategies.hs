{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}

module OhHell.Strategies where

import Prelude ()
import ClassyPrelude hiding (fromList)

import OhHell.Core
import OhHell.Pretty
import OhHell.Player
import OhHell.Rules
import qualified Data.List.NonEmpty as NonEmpty

import Control.Monad.Random (MonadRandom,getRandomR, fromList)
import qualified Data.Set as Set
import qualified Data.List as List
import Data.List ((!!))
import Game.Implement.Card.Standard (PlayingCard,Suit)

-- | Bids and plays totally randomly, and is thus next to useless
newtype RandomPlayer = RandomPlayer PlayerId deriving (Show, Eq, Ord)

instance Pretty RandomPlayer
    where prettify (RandomPlayer pid) = pid

instance Player RandomPlayer where
    getPlayerId (RandomPlayer pid) = pid
    getPlayerName (RandomPlayer pid) = pid

    chooseBid player dealerRules trumps bidsSoFar (Hand cards) = do
        let cardsThisRound = Set.size cards
            options = Set.toList $ validBids dealerRules cardsThisRound bidsSoFar
        rnd <- getRandomR (0, length options - 1)
        return $ options !! rnd

    chooseCard = playRandomCard

-- | Allow heterogeneous lists. Ugh.
data AnyPlayer = forall a. Player a => MkAnyPlayer a
deriving instance Show AnyPlayer

fromPlayer :: Player a => a -> AnyPlayer
fromPlayer = MkAnyPlayer

instance Eq AnyPlayer
    where a == b = getPlayerId a == getPlayerId b

instance Ord AnyPlayer
    where compare = comparing getPlayerId

instance Pretty AnyPlayer
    where prettify (MkAnyPlayer p) = prettify p

instance Player AnyPlayer
    where getPlayerId (MkAnyPlayer p) = getPlayerId p
          getPlayerName (MkAnyPlayer p) = getPlayerName p
          chooseBid (MkAnyPlayer p) = chooseBid p
          chooseCard (MkAnyPlayer p) = chooseCard p

-- | Plays randomly but bids randomly but weighted towards the expected outcome
newtype LessRandomPlayer = LessRandomPlayer PlayerId deriving (Show, Eq, Ord)

instance Pretty LessRandomPlayer
    where prettify (LessRandomPlayer pid) = pid

instance Player LessRandomPlayer where
    getPlayerId (LessRandomPlayer pid) = pid
    getPlayerName (LessRandomPlayer pid) = pid

    chooseBid player dealerRules trumps bidsSoFar (Hand cards) = do
        let cardsThisRound = Set.size cards
            expected = fromIntegral cardsThisRound / fromIntegral (numPlayers dealerRules)
            options = Set.toList $ validBids dealerRules cardsThisRound bidsSoFar
            weighted = weightFor <$> options
            weightFor b = (b, 1 / (1 + diff * diff)) where diff = fromIntegral b - expected
        trace ("Weights for " <> prettify player <> ": " <> prettify weighted) $ pure ()
        rnd <- fromList weighted
        return $ fst $ weighted !! rnd

    chooseCard = playRandomCard


-- | Plays a card utterly randomly
playRandomCard :: (DealerRules d, MonadRandom m, Pretty p, Player p2)
         => p
         -> d               -- ^ Rules of the game
         -> Maybe Suit      -- ^ Trumps if any
         -> BidsFor p2      -- ^ What has been bid
         -> Hand            -- ^ (What remains of) the player's hand
         -> Trick p2        -- ^ What has been played so far this trick
         -> m PlayingCard   -- ^ The chosen card
playRandomCard player dealerRules trumps bids hand played = do
        let options = Set.toList $ validCards dealerRules played hand
        trace ("Options for " <> prettify player <> ": " <> prettify options) $ pure ()
        rnd <- getRandomR (0, List.length options - 1)
        return $ options !! rnd
