{-# LANGUAGE NoImplicitPrelude #-}

module OhHell where

import ClassyPrelude

import GHC.IO (IO(IO))
--import Data.List.NonEmpty
import qualified Data.Set as Set
import Game.Implement.Card (Card, OrderedCard, fullDeck, sortCardsBy)
import Game.Implement.Card.Standard.Poker (Order(AceHighRankOrder))
import Game.Implement.Card.Standard
import Control.Monad.State (StateT, get, modify, lift)
import Text.Printf (printf)

deck :: [PlayingCard]
deck = sortCardsBy AceHighRankOrder fullDeck

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


data Player = Player {playerId :: Int, playerName :: String} deriving (Eq)
instance Show Player where
  show (Player pid name) = printf "%s <#%d>" name pid

data PlayerRoundResult = PlayerRoundResult {handBid :: Int, handTaken :: Int} deriving (Eq)
instance Show PlayerRoundResult where
  show (PlayerRoundResult bid taken) = printf "{bid:%d, got:%d}" bid taken
newtype RoundResult = RoundResult [(Player, PlayerRoundResult)] deriving (Show, Eq)
type Scores = [RoundResult]

type GameState = Scores


playRound :: StateT GameState IO ()
playRound = do
  scores <- get
  let n = length scores
  lift $ printf "On round #%d.\n" n

scoreForPlayerRound :: PlayerRoundResult -> Int
scoreForPlayerRound (PlayerRoundResult bid taken)
 | bid == taken = 10 + bid * bid
 | otherwise = -1 * abs (bid - taken)
