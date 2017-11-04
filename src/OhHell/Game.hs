module OhHell.Game where

import           Prelude ()
import           ClassyPrelude hiding ((<|))

import           OhHell.Core
import           OhHell.Rules
import           OhHell.Strategies
import           OhHell.Player
import           Control.Monad.Random (MonadRandom)
import           Control.Monad.Writer (WriterT,tell)
import           Control.Monad.State (StateT,put,get)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.List.NonEmpty (NonEmpty, NonEmpty((:|)), (<|))

import qualified Data.Map as Map
import           Text.Printf (printf)
import           Data.Map ((!))
import qualified Data.Set as Set
import qualified Data.List as List
import           Game.Implement.Card.Standard (PlayingCard,Suit,toSuit)
import           Game.Implement.Card (fullDeck,shuffle)
import           System.Random.Shuffle (shuffleM)

-- Play a round of the game
playGame :: (DealerRules d, ScorerRules s, MonadRandom m, Player p)
            => d
            -> s
            -> NonEmpty p
            -> Deck
            -> WriterT Log (StateT Results m) ()
playGame dealerRules scorerRules players startDeck = do
  results <- get
  let roundNo = List.length results + 1
  let cardsThisRound = numCardsForRound dealerRules roundNo
  unless (cardsThisRound == 0) $ do
    let top : deck = startDeck
    let numPlayers = NonEmpty.length players
    let trumps = Just (toSuit top)
    tell $ printf "----- Round: #%02d. Cards: %02d. Trumps: %s -----\n" roundNo cardsThisRound $ (show . toSuit) top
    let (playerHands, deck') = dealPlayerHands cardsThisRound deck players
    tell $ printf "Dealt: %s\n" $ show playerHands

    bidResults <- bidOnRound dealerRules cardsThisRound trumps playerHands
    tell $ printf "Bids are: %s\n" (show bidResults)

    let roundResults = fakeResultsFor bidResults
    put $ roundResults : results
    playGame dealerRules scorerRules players deck

shuffledDeck :: (MonadRandom m)
             => m Deck
shuffledDeck = shuffleM fullDeck


dealHand :: NumCards
         -> Deck
         -> (Hand, Deck)
dealHand numCards deck = (hand, newDeck)
      where hand = Hand $ NonEmpty.fromList $ take numCards deck
            newDeck = drop numCards deck

-- | Deal a hand of specified size to each player in the non-empty list given
-- Returns a non-empty list of tuples of players and their hand, plus the leftover deck
dealPlayerHands :: (Player p)
                => NumCards
                -> Deck
                -> NonEmpty p
                -> (NonEmpty (p, Hand), Deck)
dealPlayerHands numCards deck (pl :| []) = ((pl, hand) :| [], deck')
     where (hand, deck')  = dealHand numCards deck
dealPlayerHands numCards deck (pl :| ps) = ((pl, hand) <| rest, deck'')
     where (hand, deck')  = dealHand numCards deck
           (rest, deck'') = recurse deck' (NonEmpty.fromList ps)
           recurse = dealPlayerHands numCards

-- | All players bid for a round
bidOnRound :: (DealerRules d, MonadRandom m, Player p)
           => d
           -> NumCards
           -> Maybe Suit
           -> NonEmpty (p, Hand)
           -> m PlayerBids
bidOnRound dr n tr phs = bidOnRound' dr n tr (NonEmpty.toList phs) []

bidOnRound' :: (DealerRules d, MonadRandom m, Player p)
            => d
            -> NumCards
            -> Maybe Suit
            -> [(p, Hand)]
            -> PlayerBids
            -> m PlayerBids
bidOnRound' _ _ _ [] bidsSoFar = return bidsSoFar
bidOnRound' dealerRules cardsThisRound trumps playerHands bidsSoFar = do
    let (p, hand) : ps = playerHands
    newBid <- chooseBid p dealerRules trumps bidsSoFar hand
    bidOnRound' dealerRules cardsThisRound trumps ps (bidsSoFar ++ [(getPlayerId p, newBid)])

-- | Some fake results, whilst we don't have actual game logic...
fakeResultsFor :: PlayerBids -> RoundResults
fakeResultsFor playerBids = Map.fromList $ second (\b -> PlayerRoundResult {handBid=b, handTaken=0}) <$> playerBids
