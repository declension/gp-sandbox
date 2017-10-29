{-# LANGUAGE NoImplicitPrelude #-}
module OhHell.Game where

import OhHell
import OhHell.Strategies
import Control.Monad.Random (MonadRandom)
import Control.Monad.Writer (WriterT,tell)
import Control.Monad.State (StateT,put,get)
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map as Map
import Text.Printf (printf)
import ClassyPrelude
import Data.Map ((!))
import qualified Data.Set as Set
import qualified Data.List as List
import Game.Implement.Card.Standard (PlayingCard,Suit,toSuit)
import Game.Implement.Card (fullDeck,shuffle)
import System.Random.Shuffle (shuffleM)

-- Play a round of the game
playGame :: (DealerRules d, ScorerRules s, MonadRandom m, Player p)
            => d
            -> s
            -> NonEmpty p
            -> WriterT Log (StateT Results m) ()
playGame dealerRules scorerRules players = do
  results <- get
  let roundNo = len results + 1
  let cardsThisRound = numCardsForRound dealerRules roundNo
  unless (cardsThisRound == 0) $ do
    top : deck <- shuffledDeck
    --tell $ "Deck: " <> show deck
    let numPlayers = NonEmpty.length players
    let trumps = Just (toSuit top)
    tell $ printf "----- Round: #%02d. Cards: %02d. Trumps: %s -----\n" roundNo cardsThisRound $ (show . toSuit) top
    (bidResults, deck') <- bidOnRound dealerRules cardsThisRound deck trumps (NonEmpty.toList players)
    tell $ printf "Bids are: %s\n" (show bidResults)
    let roundResults = fakeResultsFor bidResults
    put $ roundResults : results
    playGame dealerRules scorerRules players

shuffledDeck :: (MonadRandom m)
             => m Deck
shuffledDeck = shuffleM fullDeck


dealHand :: (MonadRandom m)
         => NumCards
         -> Deck
         -> m (Maybe (Hand, Deck))
dealHand numCards deck
  | numCards > length deck = return Nothing
  | otherwise = return $ Just (hand, newDeck)
      where hand = Hand $ NonEmpty.fromList $ take numCards deck
            newDeck = drop numCards deck

-- | All players bid for a round
bidOnRound :: (DealerRules d, MonadRandom m, Player p)
           => d
           -> NumCards
           -> Deck
           -> Maybe Suit
           -> [p]
           -> m (PlayerBids, Deck)
bidOnRound dr n deck tr ss = bidOnRound' dr n deck tr ss []

bidOnRound' :: (DealerRules d, MonadRandom m, Player p)
            => d
            -> NumCards
            -> Deck
            -> Maybe Suit
            -> [p]
            -> PlayerBids
            -> m (PlayerBids, Deck)
bidOnRound' _ _ deck _ [] bidsSoFar = return (bidsSoFar, deck)
bidOnRound' dealerRules cardsThisRound deck trumps players bidsSoFar = do
    let p : ps = players
    -- TODO: sort this mess out
    maybeHand <- dealHand cardsThisRound deck
    let Just (hand, newDeck) = maybeHand
    newBid <- chooseBid p dealerRules trumps bidsSoFar hand
    bidOnRound' dealerRules cardsThisRound newDeck trumps ps (bidsSoFar ++ [(getPlayerId p, newBid)])

-- | Some fake results, whilst we don't have actual game logic...
fakeResultsFor :: PlayerBids -> RoundResults
fakeResultsFor playerBids = Map.fromList $ second (\b -> PlayerRoundResult {handBid=b, handTaken=0}) <$> playerBids
