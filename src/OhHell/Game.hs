{-# LANGUAGE NoImplicitPrelude #-}
module OhHell.Game where

import OhHell
import OhHell.Strategy
import Control.Monad.Random (MonadRandom)
import Control.Monad.Writer (WriterT,tell)
import Control.Monad.State (StateT,put,get)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import Text.Printf (printf)
import ClassyPrelude
import Data.Map ((!))
import qualified Data.Set as Set
import qualified Data.List as List
import Game.Implement.Card.Standard (PlayingCard)
import Game.Implement.Card (fullDeck,shuffle)
import System.Random.Shuffle (shuffleM)

-- Play a round of the game
playGame :: (DealerRules d, ScorerRules s, MonadRandom m)
            => d
            -> s
            -> WriterT Log (StateT Results m) ()
playGame dealerRules scorerRules = do
  results <- get
  let roundNo = ((+1) . len . snd . NonEmpty.head) results
  let cardsThisRound = numCardsForRound dealerRules roundNo
  unless (cardsThisRound == 0) $ do
    deck <- shuffledDeck
    tell $ "Deck: " <> show deck
    let players = NonEmpty.map fst results
    let numPlayers = NonEmpty.length results
    tell $ printf "--- round #%02d (with %d players) ---\n" roundNo numPlayers

    -- bid
    tell $ printf "Dealing %d card(s)...\n" cardsThisRound
    (bidResults, deck') <- bidOnRound dealerRules cardsThisRound deck (NonEmpty.toList players)
    tell $ show bidResults <> "\n"
    let roundResults = fakeResultsFor bidResults
    let results' = updatePlayer <$> results
          where updatePlayer (p, history) = (p, roundResults ! p : history)
    put results'
    playGame dealerRules scorerRules

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
bidOnRound :: (DealerRules d, MonadRandom m)
           => d
           -> NumCards
           -> Deck
           -> [Player]
           -> m (PlayerBids, Deck)
bidOnRound dr n deck ps = bidOnRound' dr n deck ps []

bidOnRound' :: (DealerRules d, MonadRandom m)
            => d
            -> NumCards
            -> Deck
            -> [Player]
            -> PlayerBids
            -> m (PlayerBids, Deck)
bidOnRound' _ _ deck [] bidsSoFar = return (bidsSoFar, deck)
bidOnRound' dealerRules cardsThisRound deck players bidsSoFar = do
    let p : ps = players
    maybeHand <- dealHand cardsThisRound deck
    let Just (hand, newDeck) = maybeHand
    newBid <- calculateBid RandomBidder dealerRules p bidsSoFar hand
    bidOnRound' dealerRules cardsThisRound newDeck ps (bidsSoFar ++ [(p, newBid)])

-- | Some fake results, whilst we don't have actual game logic...
fakeResultsFor :: PlayerBids -> RoundResults
fakeResultsFor playerBids = Map.fromList $ second (\b -> PlayerRoundResult {handBid=b, handTaken=0}) <$> playerBids
