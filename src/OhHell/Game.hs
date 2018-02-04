module OhHell.Game where

import           Prelude ()
import           ClassyPrelude hiding ((<|))

import           OhHell.Core
import           OhHell.Pretty
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
    tell $ printf "----- Round: #%02d. Cards: %02d. Trumps: %s -----\n" roundNo cardsThisRound $ (prettify . toSuit) top
    let (playerHands, deck') = dealPlayerHands cardsThisRound deck players
    tell $ printf "Dealt: %s\n" $ prettify playerHands

    -- Bid
    bids <- bidOnRound dealerRules trumps playerHands
    tell $ printf "Bids are: %s\n" (prettify bids)

    -- Play round
    roundResults <- playRound dealerRules trumps bids playerHands
    tell $ printf "Round results: %s\n" (prettify results)

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
      where hand = Hand $ Set.fromList $ take numCards deck
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

-- | Is the round finished, in any way
finished :: (Player p) => HandsFor p -> Bool
finished [] = True
finished ((p, Hand hand) : _) = Set.null hand

-- | All players bid for a round
bidOnRound :: (DealerRules d, MonadRandom m, Player p)
           => d
           -> Maybe Suit
           -> NonEmpty (p, Hand)
           -> m (BidsFor p)
bidOnRound dr tr phs = bidOnRound' dr tr (NonEmpty.toList phs) []

-- | Internal recursive version
bidOnRound' :: (DealerRules d, MonadRandom m, Player p)
            => d
            -> Maybe Suit
            -> HandsFor p
            -> BidsFor p
            -> m (BidsFor p)
bidOnRound' _ _ [] bidsSoFar = return bidsSoFar
bidOnRound' dealerRules trumps playerHands bidsSoFar = do
    let (p, Hand hand) : ps = playerHands
        cardsThisRound = Set.size hand
    newBid <- chooseBid p dealerRules trumps bidsSoFar (Hand hand)
    bidOnRound' dealerRules trumps ps (bidsSoFar ++ [(p, newBid)])

-- | Play a complete round of tricks for each of the passed players for each of their cards
playRound :: (DealerRules d, MonadRandom m, Player p)
            => d
            -> Maybe Suit
            -> BidsFor p
            -> NonEmpty (p, Hand)
            -> m (RoundResultsBy p)
playRound dealerRules trumps bids playerHands = trace ("Playing round of " ++ prettify playerHands) $ fmap Map.fromList results
    where results = playRound' dealerRules trumps bids (NonEmpty.toList playerHands) Map.empty

-- | Internal recursive implementation
playRound' :: (DealerRules d, MonadRandom m, Player p)
           => d
           -> Maybe Suit
           -> BidsFor p
           -> HandsFor p
           -> TakenBy p
           -> m (RoundResultsFor p)
playRound' dealerRules trumps bids playerHands taken
  | finished playerHands = trace ("Finished round! Tricks taken: " ++ prettify taken) return $ map zipper bids
  | otherwise            = do
    trace ("Trumps are " ++ prettify trumps ++ ". Hands:" ++ prettify playerHands) return ()
    (trick, newHands) <- playTrick' dealerRules trumps bids playerHands ([], [])
    let winner = trickWinnerFor trumps trick
        -- Rotate players according to winner
        newOrderedHands = winnerOrderedFor winner newHands
    trace (prettify winner ++ " won. New order: " ++ prettify newOrderedHands) pure ()
    playRound' dealerRules trumps bids newOrderedHands (Map.alter inc winner taken)
    where inc Nothing  = Just 1
          inc (Just x) = Just (x + 1)
          zipper (p, bid) = (p, RoundResult {handTaken = fromMaybe 0 (Map.lookup p taken), handBid = bid})


-- | Rotate list of player hands according to the winner
winnerOrderedFor :: (Eq p)
                 => p
                 -> [(p, a)]
                 -> [(p, a)]
winnerOrderedFor winner hands
  | winner `elem` players = take numPlayers $ dropWhile isNotWinner cycled
  -- Could error, but purer just to return the original I guess
  | otherwise = hands
    where players = fst <$> hands
          numPlayers = List.length hands
          isNotWinner = (/= winner) . fst
          cycled = List.cycle hands

-- | Play a single trick for each of the passed players
playTrick :: (DealerRules d, MonadRandom m, Player p)
            => d
            -> Maybe Suit
            -> BidsFor p
            -> NonEmpty (p, Hand)
            -> m (Trick p, HandsFor p)
playTrick dealerRules trumps bids playerHands
    = playTrick' dealerRules trumps bids (NonEmpty.toList playerHands) ([], [])

-- | Internal recursive implementation
-- Mutates the playerHands across iterations building up (cards, hands) state that starts empty.
-- An empty playerHands terminates the recursion
playTrick' :: (DealerRules d, MonadRandom m, Player p)
            => d
            -> Maybe Suit
            -> BidsFor p
            -> HandsFor p
            -> (Trick p, HandsFor p)
            -> m (Trick p, HandsFor p)
playTrick' dealerRules trumps bids playerHands (cardsSoFar, handsSoFar)
  | finished playerHands = return (cardsSoFar, handsSoFar)
  | otherwise            = do
    let (p, Hand hand) : phs = playerHands
        cardsThisRound = Set.size hand
    chosenCard <- chooseCard p dealerRules trumps bids (Hand hand) cardsSoFar
    trace (prettify p ++ " played " ++ prettify chosenCard) pure ()
    let newCardsSoFar = cardsSoFar ++ [(p, chosenCard)]
        newHandsSoFar = handsSoFar ++ [(p, Hand newHand)]
        newHand = Set.delete chosenCard hand
    playTrick' dealerRules trumps bids phs (newCardsSoFar, newHandsSoFar)

