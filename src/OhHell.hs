{-# LANGUAGE NoImplicitPrelude #-}

module OhHell where

import ClassyPrelude
import GHC.IO (IO(IO))

import qualified Data.Set as Set
import Game.Implement.Card (Card, OrderedCard, fullDeck, sortCardsBy)
import Game.Implement.Card.Standard.Poker (Order(AceHighRankOrder))
import Game.Implement.Card.Standard
import Control.Monad.State (StateT, get, put, modify, lift)
import Text.Printf (printf)

deck :: NonNull [PlayingCard]
deck = impureNonNull $ sortCardsBy AceHighRankOrder fullDeck

newtype Hand = Hand (Set PlayingCard)
  deriving (Eq)

-- Make constructing cards prettier, e.g. @Ace ## Spades@
(##) = PlayingCard
infix 8 ##

-- Make showing Hands prettier
instance Show Hand where
  show (Hand cards) = "{" <> intercalate ", " cardList <> "}"
    where cardList = show <$> toList cards

handOf :: [PlayingCard] -> Hand
handOf lst = Hand $ Set.fromList lst

-- Basic Player information
data Player = Player {playerId :: Int, playerName :: String} deriving (Eq, Ord)
-- ...with pretty printing
instance Show Player where
  show (Player pid name) = printf "%s <#%d>" name pid

data PlayerRoundResult = PlayerRoundResult {handBid :: Int, handTaken :: Int} deriving (Eq, Ord)
instance Show PlayerRoundResult where
  show (PlayerRoundResult bid taken) = printf "{bid:%d, got:%d}" bid taken

-- The results round-by-round, broken down by player
type Results = NonNull [(Player, [PlayerRoundResult])]
type Scores = NonNull [(Player, Score)]

-- Some aliases for readability
type Bid   = Int
type Taken = Int
type Score = Int
type RoundNum = Int
type NumCards = Int

-- The state we need to track throughout the game
type GameState = (Results, RoundNum)


class ScorerRules a where
  scoreForPlayerRound :: a -> PlayerRoundResult -> Score

data ProgressiveScoring = ProgressiveScoring {flatBonus :: Int, penaltyFactor :: Int}

instance ScorerRules ProgressiveScoring where
  scoreForPlayerRound sr (PlayerRoundResult bid taken)
   | bid == taken = flatBonus sr + bid * bid
   | otherwise    = penaltyFactor sr * abs (bid - taken)

scoresFor :: ScorerRules a =>
      a ->
      Results ->
      Scores
-- whyyyyyyyyyyyyyyy?
scoresFor rules results = impureNonNull $ second (totalScore rules) <$> toNullable results


-- This can definitely be empty
totalScore :: ScorerRules a => a -> [PlayerRoundResult] -> Score
totalScore rules = sum . fmap (scoreForPlayerRound rules)

-- Rules (variations) governing playing
-- Notably: 1. Number of cards dealt per round
--          2. What constitutes a valid bid for a player on a round (e.g. bid-busting)
class DealerRules dr where
  numCards :: dr -> RoundNum -> NumCards
  validBids :: dr -> [Player] -> RoundNum -> Player -> [Bid]


-- Choose a bid for the given hand.
-- 'options' is the bids available, as determined by the rule system
chooseBid :: Results -> Hand -> NonNull [Bid] -> Bid
chooseBid results hand = head


-- Play a round of the game
playRound :: ScorerRules s => s -> StateT GameState IO ()
playRound scorerRules = do
  (scores, roundNum) <- get
  let numPlayers = length scores
  lift $ printf "On round #%d with %d players.\n" (length $ snd $ head scores) numPlayers
  put (scores, roundNum + 1)

