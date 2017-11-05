module OhHell.Core where

import           Prelude ()
import           ClassyPrelude hiding (intercalate)
import           Data.List (intercalate)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map (Map)
import           Game.Implement.Card.Standard (PlayingCard(PlayingCard))
import           Text.Printf (printf)

newtype Hand =
  Hand (NonEmpty PlayingCard)
  deriving (Eq)

-- | Make constructing cards prettier, e.g. @Ace ## Spades@
( ## ) = PlayingCard

infix 8 ##

-- Make showing Hands prettier
instance Show Hand where
  show (Hand cards) = "{" <> intercalate ", " cardList <> "}"
    where
      cardList = show <$> NonEmpty.toList cards

handOf :: [PlayingCard] -> Hand
handOf lst = Hand $ NonEmpty.fromList lst

data RoundResult = RoundResult
  { handBid :: Bid
  , handTaken :: Taken
  } deriving (Eq, Ord)

instance Show RoundResult where
  show (RoundResult bid taken) = printf "{bid:%d, got:%d}" bid taken

-- | The results round-by-round, broken down by player
type PlayerId = String

type RoundResultsBy p = Map p RoundResult

type ResultsFor p = [RoundResultsBy p]

type ScoresBy p = Map p Score

type BidsFor p = [(p, Bid)]

type CardsFor p = [(p, PlayingCard)]

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
