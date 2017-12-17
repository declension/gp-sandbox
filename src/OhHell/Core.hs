module OhHell.Core where

import           Prelude ()
import           ClassyPrelude hiding (intercalate)

import           OhHell.Pretty
import           Data.List (intercalate)
import qualified Data.Set as Set
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map (Map)
import           Game.Implement.Card.Standard (PlayingCard(PlayingCard))
import           Text.Printf (printf)

newtype Hand = Hand {fromHand :: Set PlayingCard}
    deriving (Eq, Show)

instance Pretty Hand
    where prettify (Hand set) = prettify set

-- | Make constructing cards prettier, e.g. @Ace ## Spades@
( ## ) = PlayingCard

infix 8 ##


handOf :: [PlayingCard] -> Hand
handOf lst = Hand $ Set.fromList lst
emptyHand = Hand Set.empty

data RoundResult = RoundResult
  { handBid :: Bid
  , handTaken :: Taken
  } deriving (Eq, Ord, Show)

instance Pretty RoundResult where
  prettify (RoundResult bid taken) = printf "{bid %d, got %d}" bid taken

-- | The results round-by-round, broken down by player
type PlayerId = String

type RoundResultsBy p = Map p RoundResult

type RoundResultsFor p = [(p, RoundResult)]

type ResultsFor p = [RoundResultsBy p]

type ScoresBy p = Map p Score

type BidsFor  p = [(p, Bid)]
type TakenFor p = [(p, Taken)]
type TakenBy  p = Map p Taken

type CardsFor p = [(p, PlayingCard)]
type Trick p = CardsFor p

type HandsFor p = [(p, Hand)]

-- Some aliases for readability
type Deck = [PlayingCard]

type Bid = Int

type Taken = Int

type Score = Int

type RoundNum = Int

type NumRounds = Int

type NumCards = Int

type NumPlayers = Int

type Log = String
