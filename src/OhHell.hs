

module OhHell where

import Prelude (($), (.), Show, Eq, show, flip, String, Int, putStrLn, show)
import GHC.IO (IO(IO))
--import Data.List.NonEmpty
import Data.List (intercalate)
import Data.Monoid ((<>))
import Data.Functor ((<$>))
import Data.Set
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
instance Prelude.Show Hand where
  show (Hand cards) = "{" <> intercalate ", " cardList <> "}"
    where cardList = show <$> toList cards

handOf :: [PlayingCard] -> Hand
handOf lst = Hand $ fromList lst


data Player = Player {playerId :: Int, playerName :: String} deriving (Eq)
instance Show Player where
  show (Player pid name) = printf "%s <#%d>" name pid

data HandResult = HandResult {handBid :: Int, handTaken :: Int} deriving (Eq)
instance Show HandResult where
  show (HandResult bid taken) = printf "{bid:%d, got:%d}" bid taken
newtype RoundResult = RoundResult [(Player, HandResult)] deriving (Show, Eq)
type Scores = [RoundResult]

type GameState = Scores


playGame :: StateT GameState IO ()
playGame = do
  scores <- get
  lift $ putStrLn "Got something"
