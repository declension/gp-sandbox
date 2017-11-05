module OhHell.Game where

import           Prelude ()
import           ClassyPrelude hiding ((<|))

import           OhHell.Core
import           OhHell.Rules
import           OhHell.Strategies
import           OhHell.Player
import           Control.Monad.Random (MonadRandom)
import           Control.Monad.Writer (WriterT,tell,execWriterT)
import           Control.Monad.State (StateT,put,get,runStateT)
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


-- | Run an entire game from start to finish
runGame :: (Player p, MonadRandom m, DealerRules d, ScorerRules s)
        => d
        -> s
        -> NonEmpty p
        -> Deck
        -> m (Log, ResultsFor p)
runGame dealingRules scoringRules players deck = runStateT (execWriterT playConfiguredGame) initialResults
  where playConfiguredGame = playGame dealingRules scoringRules players deck
        initialResults = []


-- | Play a round of the game
playGame :: (DealerRules d, ScorerRules s, MonadRandom m, Player p)
            => d
            -> s
            -> NonEmpty p
            -> Deck
            -> WriterT Log (StateT (ResultsFor p) m) ()
playGame dealerRules scorerRules players startDeck = do
  results <- get
  let roundNo = List.length results + 1
  let cardsThisRound = numCardsForRound dealerRules roundNo
  unless (cardsThisRound == 0) $ do
    -- Deal cards
    let top : deck = startDeck
    let trumps = Just (toSuit top)
    let numPlayers = NonEmpty.length players
    tell $ printf "----- Round: #%02d. Cards: %02d. Trumps: %s -----\n" roundNo cardsThisRound $ (show . toSuit) top
    let (playerHands, deck') = dealPlayerHands cardsThisRound deck players
    tell $ printf "Dealt: %s\n" $ show playerHands

    -- Bid
    bidResults <- bidOnRound dealerRules trumps playerHands
    tell $ printf "Bids are: %s\n" (show bidResults)

    -- Play trick
    let roundResults = fakeResultsFor bidResults
--    roundResults <- playRound dealerRules trumps playerHands

    -- Store results
    put $ roundResults : results

    -- Recurse!
    playGame dealerRules scorerRules players deck


-- | Return a new shuffled deck
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
           -> Maybe Suit
           -> NonEmpty (p, Hand)
           -> m (BidsFor p)
bidOnRound dr tr phs = bidOnRound' dr tr (NonEmpty.toList phs) []

bidOnRound' :: (DealerRules d, MonadRandom m, Player p)
            => d
            -> Maybe Suit
            -> HandsFor p
            -> BidsFor p
            -> m (BidsFor p)
bidOnRound' _ _ [] bidsSoFar = return bidsSoFar
bidOnRound' dealerRules trumps playerHands bidsSoFar = do
    let (p, Hand hand) : ps = playerHands
        cardsThisRound = NonEmpty.length hand
    newBid <- chooseBid p dealerRules trumps bidsSoFar (Hand hand)
    bidOnRound' dealerRules trumps ps (bidsSoFar ++ [(p, newBid)])

-- | Some fake results, whilst we don't have actual game logic...
fakeResultsFor :: (Player p) => BidsFor p -> RoundResultsBy p
fakeResultsFor playerBids = Map.fromList $ second (\b -> RoundResult {handBid=b, handTaken=0}) <$> playerBids
