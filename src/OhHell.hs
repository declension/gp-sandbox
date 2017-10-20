{-# LANGUAGE NoImplicitPrelude #-}

module OhHell where
import qualified Data.List as List
import ClassyPrelude hiding (fromList)

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map (fromList)
import Data.Map ((!), Map)
import qualified Data.Set as Set
import Game.Implement.Card (Card, OrderedCard, fullDeck, sortCardsBy)
import Game.Implement.Card.Standard.Poker (Order(AceHighRankOrder))
import Game.Implement.Card.Standard
import Control.Monad.State (StateT, get, put, modify, lift,execStateT,State,execState)
import Control.Monad (unless)
import Text.Printf (printf)
import Data.Set (Set)
import Data.Semigroup ((<>))
import Control.Monad.Random (RandomGen, RandT, getRandomR, mkStdGen, evalRandT,Rand,MonadRandom,evalRand,getStdGen)

len = List.length
--import Prelude (Int, Show, Eq, Ord, String, show, ($), (.), (<$>), fmap, otherwise, sum, (==), (+), (*), (-), (abs))

deck :: NonEmpty PlayingCard
deck = fromList $ sortCardsBy AceHighRankOrder fullDeck

newtype Hand = Hand (Set PlayingCard)
  deriving (Eq)

-- Make constructing cards prettier, e.g. @Ace ## Spades@
(##) = PlayingCard
infix 8 ##

-- Make showing Hands prettier
instance Show Hand where
  show (Hand cards) = "{" <> intercalate ", " cardList <> "}"
    where cardList = show <$> Set.toList cards

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
type Results = NonEmpty (Player, [PlayerRoundResult])
type Scores = NonEmpty (Player, Score)
type RoundResults = Map Player PlayerRoundResult

-- Some aliases for readability
type Bid   = Int
type Taken = Int
type Score = Int
type RoundNum = Int
type NumCards = Int
type NumPlayers = Int


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
scoresFor rules results = second (totalScore rules) <$> results


-- This can definitely be empty
totalScore :: ScorerRules a => a -> [PlayerRoundResult] -> Score
totalScore rules = sum . fmap (scoreForPlayerRound rules)

-- Rules (variations) governing playing
-- Notably: 1. Number of cards dealt per round
--          2. What constitutes a valid bid for a player on a round (e.g. bid-busting)
class DealerRules dr where
  numPlayers :: dr -> NumPlayers
  numCardsForRound :: dr -> RoundNum -> NumCards

  -- Is bid-busting (last bidder can't allow a total bid equal to hand size) enabled?
  bidBusting :: dr -> Bool

  -- What are all the valid bids given the previous bids
  validBids :: dr -> NumCards -> PlayerBids -> Set Bid
  validBids rules numCards bids
    | bidBusting rules && len bids == (numPlayers rules - 1) =  Set.difference allBids disallowed
    | otherwise = allBids
    where allBids = Set.fromList [0..numCards]
          disallowed = Set.singleton $ numCards - sum (List.map snd bids)

data RikikiDealing = RikikiDealingFor Int

instance DealerRules RikikiDealing where
  numPlayers (RikikiDealingFor n) = n
  numCardsForRound dr roundNum = (up ++ maxRoundSize : down ++ zeros) List.!! (roundNum - 1)
                    where maxRoundSize = 51 `div` numPlayers dr
                          up = [1..maxRoundSize-1]
                          down = List.reverse up
                          zeros = List.repeat 0
  bidBusting dr = True


-- Play a round of the game
playGame :: (DealerRules d, ScorerRules s) => d -> s -> StateT Results IO ()
playGame dealerRules scorerRules = do
  results <- get
  let roundNo = ((+1) . len . snd . NonEmpty.head) results
  let cardsThisRound = numCardsForRound dealerRules roundNo
  if cardsThisRound == 0 then return () else do
    let players = NonEmpty.map fst results
    let numPlayers = NonEmpty.length results
    liftIO $ printf "--- round #%02d (with %d players) ---\n" roundNo numPlayers

    -- bid
    liftIO $ printf "Dealing %d card(s)...\n" cardsThisRound
    ng <- liftIO getStdGen
    let bidResults = evalRand (bidOnRound dealerRules cardsThisRound (NonEmpty.toList players)) ng
    print bidResults
    let roundResults = fakeResultsFor bidResults
    let results' = updatePlayer <$> results
          where updatePlayer (p, history) = (p, roundResults ! p : history)
    put results'
    playGame dealerRules scorerRules

type PlayerBids = [(Player, Bid)]

-- | All players bid for a round
bidOnRound :: (DealerRules d, MonadRandom m) => d -> NumCards -> [Player] -> m PlayerBids
bidOnRound d n ps = bidOnRound' d n ps []

bidOnRound' :: (DealerRules d, MonadRandom m) => d -> NumCards -> [Player] -> PlayerBids -> m PlayerBids
bidOnRound' _ _ [] bidsSoFar = return bidsSoFar
bidOnRound' dealerRules cardsThisRound players bidsSoFar = do
    let options = Set.toList $ validBids dealerRules cardsThisRound bidsSoFar
    rnd <- getRandomR (0, len options - 1)
    let newBid = options List.!! rnd
    let p : ps = players
    bidOnRound' dealerRules cardsThisRound ps (bidsSoFar ++ [(p, newBid)])


fakeResultsFor :: [(Player, Bid)] -> RoundResults
fakeResultsFor playerBids = Map.fromList $ second (\b -> PlayerRoundResult {handBid=b, handTaken=0}) <$> playerBids
