{-# LANGUAGE NoImplicitPrelude #-}

module OhHell where

import qualified Data.List as List
import ClassyPrelude hiding (fromList)

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map (fromList)
import Data.Map ((!), Map)
import qualified Data.Set as Set
import Game.Implement.Card (Card, OrderedCard)
import Game.Implement.Card.Standard.Poker (Order(AceHighRankOrder))
import Game.Implement.Card.Standard
import Control.Monad.State (StateT, get, put, modify, lift,execStateT,State,execState)
import Control.Monad (unless)
import Text.Printf (printf)
import Data.Set (Set)
import Data.Semigroup ((<>))
import Control.Monad.Random (RandomGen, RandT, getRandomR, mkStdGen, evalRandT,Rand,MonadRandom,evalRand,getStdGen)
import Control.Monad.Writer (WriterT,tell)

len = List.length

newtype Hand = Hand (NonEmpty PlayingCard)
  deriving (Eq)

-- | Make constructing cards prettier, e.g. @Ace ## Spades@
(##) = PlayingCard
infix 8 ##

-- Make showing Hands prettier
instance Show Hand where
  show (Hand cards) = "{" <> intercalate ", " cardList <> "}"
    where cardList = show <$> NonEmpty.toList cards

handOf :: [PlayingCard] -> Hand
handOf lst = Hand $ NonEmpty.fromList lst

-- | Basic Player information
data Player = Player {playerId :: Int, playerName :: String} deriving (Eq, Ord)
-- ...with pretty printing
instance Show Player where
  show (Player pid name) = printf "%s <#%d>" name pid

data PlayerRoundResult = PlayerRoundResult {handBid :: Bid, handTaken :: Taken}
  deriving (Eq, Ord)
instance Show PlayerRoundResult where
  show (PlayerRoundResult bid taken) = printf "{bid:%d, got:%d}" bid taken

-- | The results round-by-round, broken down by player
type Results = NonEmpty (Player, [PlayerRoundResult])
type Scores = NonEmpty (Player, Score)
type RoundResults = Map Player PlayerRoundResult
type PlayerBids = [(Player, Bid)]

-- Some aliases for readability
type Deck  = [PlayingCard]
type Bid   = Int
type Taken = Int
type Score = Int
type RoundNum = Int
type NumCards = Int
type NumPlayers = Int
type Log = String


class ScorerRules a where
  scoreForPlayerRound :: a -> PlayerRoundResult -> Score

data ProgressiveScoring = ProgressiveScoring {flatBonus :: Int, penaltyFactor :: Int}

instance ScorerRules ProgressiveScoring where
  scoreForPlayerRound sr (PlayerRoundResult bid taken)
   | bid == taken = flatBonus sr + bid * bid
   | otherwise    = penaltyFactor sr * abs (bid - taken)

scoresFor :: ScorerRules s
          => s
          -> Results
          -> Scores
scoresFor rules results = second (totalScore rules) <$> results


-- | Get the scores for some results.
totalScore :: ScorerRules s
           => s
           -> [PlayerRoundResult]      -- ^ This can definitely be empty
           -> Score
totalScore rules = sum . fmap (scoreForPlayerRound rules)

-- | Rules (variations) governing playing
-- Notably: 1. Number of cards dealt per round
--          2. What constitutes a valid bid for a player on a round (e.g. bid-busting)
class DealerRules dr where
  -- | Number of players in this rule set
  numPlayers :: dr -> NumPlayers

  -- | Number of cards to deal in the given round
  numCardsForRound :: dr -> RoundNum -> NumCards

  -- | Is bid-busting (last bidder can't allow a total bid equal to hand size) enabled?
  bidBusting :: dr -> Bool

  -- | What are all the valid bids given the previous bids
  validBids :: dr -> NumCards -> PlayerBids -> Set Bid
  validBids rules numCards bids
    | numCards <= 0 = Set.empty
    | bidBusting rules && len bids == (numPlayers rules - 1) =  Set.difference allBids disallowed
    | otherwise = allBids
    where allBids = Set.fromList [0..numCards]
          disallowed = Set.singleton $ numCards - sum (List.map snd bids)

-- | Rikiki-style dealing, for a given number of players
newtype RikikiDealing = RikikiDealingFor NumPlayers

instance DealerRules RikikiDealing where
  numPlayers (RikikiDealingFor n) = n
  numCardsForRound dr roundNum = (up ++ maxRoundSize : down ++ zeros) List.!! (roundNum - 1)
                    where maxRoundSize = 51 `div` numPlayers dr
                          up = [1..maxRoundSize-1]
                          down = List.reverse up
                          zeros = List.repeat 0
  bidBusting dr = True
