module GameSpec where

import Prelude ()
import ClassyPrelude

import OhHell.Core
import OhHell.Pretty
import OhHell.Rules
import OhHell.Player
import OhHell.Strategies (RandomPlayer(RandomPlayer))
import OhHell.Game (bidOnRound,playGame, runGame,playRound,playTrick,winnerOrderedFor)

import Test.Hspec
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty (fromList)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad.State (execState,runStateT)
import Control.Monad.Random (mkStdGen,evalRandT,evalRand)
import Game.Implement.Card (fullDeck)
import Control.Monad.Writer (execWriterT)
import Game.Implement.Card.Standard


spec :: Spec
spec = do
  basicSpec
  orderingSpec
  winnerOrderedSpec
  dealingSpec
  scoringSpec
  biddingSpec
  strategySpec
  trickPlayingSpec
  roundPlayingSpec
  integrationSpec

alice = RandomPlayer "Alice"
bob = RandomPlayer "Bob"
charlie = RandomPlayer "Charlie"
rr = RoundResult

basicSpec :: Spec
basicSpec =
  describe "The game engine" $ do

    it "constructs players properly" $
      getPlayerName charlie `shouldBe` "Charlie"

    it "shows players nicely" $
      prettify alice `shouldContain` "Alice"

    it "constructs HandResults " $ do
      let handResult = RoundResult 1 2
      handBid handResult `shouldBe` 1
      handTaken handResult `shouldBe` 2

    it "prettifies cards" $ do
        prettify (Five ## Hearts)   `shouldBe` "5♥"
        prettify (Ace ## Diamonds)  `shouldBe` "A♦"
        prettify (Jack ## Clubs)    `shouldBe` "J♣"
        prettify (Ten ## Spades)    `shouldBe` "10♠"

scoringSpec :: Spec
scoringSpec = describe "Scoring" $

    it "sums scores correctly for Progressive Scoring" $ do
      -- A combination of scores that should show everything
      let results = [ Map.fromList [(alice, rr 0 0), (bob, rr 0 1), (charlie, rr 0 0)]
                    , Map.fromList [(alice, rr 1 0), (bob, rr 0 0), (charlie, rr 1 1)]
                    , Map.fromList [(alice, rr 3 3), (bob, rr 0 0), (charlie, rr 1 0)]
                    ]

      let rules = ProgressiveScoring 10 (-1)
      scoresFor rules results `shouldBe` Map.fromList [(alice, 28),
                                                       (bob, 19),
                                                       (charlie, 20)]

orderingSpec :: Spec
orderingSpec = describe "Ordering" $ do
    it "orders cards correcting with no trumps" $ do
        let a = Four ## Diamonds
            b = Five ## Diamonds
            c = King ## Clubs
            trumps = Nothing
        higherCard trumps Diamonds a b `shouldBe` b
        higherCard trumps Diamonds a c `shouldBe` a

    it "orders cards correctly with trumps" $ do
        let a = Four  ## Diamonds
            b = Three ## Hearts
            c = King  ## Clubs
            d = Four  ## Hearts
            trumps = Just Hearts
            lead   = Diamonds
            winner = higherCard trumps lead
        winner a b `shouldBe` b
        winner b d `shouldBe` d
        winner b c `shouldBe` b

winnerOrderedSpec :: Spec
winnerOrderedSpec = describe "Ordering" $ do
    it "copes with empty lists" $
        winnerOrderedFor "foo" ([]  :: HandsFor a) `shouldBe` []

    it "copes with non-existent players" $
        winnerOrderedFor "baz" [("foo", 1)] `shouldBe` [("foo", 1)]

    it "copes with empty lists" $ do
        winnerOrderedFor "foo" hands `shouldBe` [("foo", 1) , ("bar", 2), ("baz", 3)]
        winnerOrderedFor "bar" hands `shouldBe` [("bar", 2), ("baz", 3), ("foo", 1)]
        winnerOrderedFor "baz" hands `shouldBe` [("baz", 3), ("foo", 1), ("bar", 2)]
        where hands =    [("foo", 1) , ("bar", 2), ("baz", 3)]


dealingSpec :: Spec
dealingSpec = describe "Dealing" $ do

    it "deals correctly for Rikiki Rules" $ do
      let dealer = RikikiDealingFor 5
      numCardsForRound dealer 1 `shouldBe` 1
      numCardsForRound dealer 2 `shouldBe` 2
      numCardsForRound dealer 9 `shouldBe` 9
      numCardsForRound dealer 10 `shouldBe` 10
      numCardsForRound dealer 11 `shouldBe` 9
      numCardsForRound dealer 19 `shouldBe` 1

    it "deals 0 eventually and forever" $ do
      let dealer = RikikiDealingFor 8
      numCardsForRound dealer 6 `shouldBe` 6
      numCardsForRound dealer 12 `shouldBe` 0
      numCardsForRound dealer 1000 `shouldBe` 0

    it "computes valid bids correctly" $ do
      let dealer = RikikiDealingFor 3
      validBids dealer 2 [] `shouldBe` Set.fromList [0, 1, 2]

    it "allows empty bids " $ do
      let dealer = RikikiDealingFor 3
      validBids dealer 0 [] `shouldBe` Set.empty

    it "supports bid busting" $ do
      let dealer = RikikiDealingFor 3
      validBids dealer 4 [(alice, 0), (bob, 1)] `shouldBe` Set.fromList [0, 1, 2, 4]

    it "validates playable cards correctly" $ do
        let
            dealer = RikikiDealingFor 3
            aliceHand   = handOf [Four ## Spades, Seven ## Diamonds]
            bobHand     = handOf [Jack ## Diamonds, Three ## Clubs]
            charlieHand = handOf [Queen ## Hearts, Two ## Hearts]
            hands = NonEmpty.fromList [(alice, aliceHand), (bob, bobHand), (charlie, charlieHand)]
            valid = validCards dealer
        valid [] aliceHand `shouldBe` fromHand aliceHand
        valid [(alice, Seven ## Diamonds)] bobHand `shouldBe` Set.singleton (Jack ## Diamonds)
        valid [(alice, Seven ## Diamonds), (bob, Jack ## Diamonds)] charlieHand `shouldBe` fromHand charlieHand


biddingSpec :: Spec
biddingSpec = describe "Dealing and bidding for a round" $
    it "should return the right number of results" $ do
      let dealer = RikikiDealingFor 3
      let playerHands = NonEmpty.fromList
                        [(alice, handOf []),
                         (bob, handOf []),
                         (charlie, handOf [])] :: NonEmpty (RandomPlayer, Hand)
      let g = mkStdGen 0
      let results = evalRand (bidOnRound dealer Nothing playerHands) g
      List.length results `shouldBe` 3


strategySpec :: Spec
strategySpec = describe "RandomStrategy" $
    it "should respect dealer" $ do
      let dealer = RikikiDealingFor 3
      let player = alice
      let numCards = 4
      let cards = Set.fromList $ take numCards fullDeck
      let trumps = Nothing
      bid <- chooseBid player dealer trumps [(bob, 2), (charlie, 1)] (Hand cards)
      bid `shouldSatisfy` (<= numCards)
      bid `shouldSatisfy` (>= 0)
      bid `shouldSatisfy` (/= 1)


roundPlayingSpec :: Spec
roundPlayingSpec = describe "Playing a round" $
    it "gives correct results" $ do
        let dealer = RikikiDealingFor 3
            gen = mkStdGen 0
            numCards = 2
            cards = NonEmpty.fromList $ take numCards (fullDeck :: [PlayingCard])
            trumps = Nothing
            bids = [(alice, 1), (bob, 2), (charlie, 0)]
            hands = NonEmpty.fromList [(alice, handOf [Four ## Spades, Seven ## Diamonds]),
                                       (bob, handOf [Jack ## Diamonds, Three ## Clubs]),
                                       (charlie, handOf [Queen ## Hearts, Four ## Diamonds])]
        let results = evalRand (playRound dealer trumps bids hands) gen
        length results `shouldBe` 3
        results `shouldBe` Map.fromList [(alice, rr 1 0), (bob, rr 2 2), (charlie, rr 0 0)]

trickPlayingSpec :: Spec
trickPlayingSpec = describe "Playing a trick" $
    it "removes played cards from hands" $ do
        let dealer = RikikiDealingFor 3
            numCards = 1
            cards = NonEmpty.fromList $ take numCards (fullDeck :: [PlayingCard])
            trumps = Nothing
            bids = [(alice, 1), (bob, 1), (charlie, 0)]
            hands = NonEmpty.fromList [(alice, handOf [Four ## Spades]),
                                       (bob, handOf [Jack ## Diamonds]),
                                       (charlie, handOf [Queen ## Hearts])]
        (cards, hands) <- playTrick dealer trumps bids hands
        length cards `shouldBe` 3
        cards `shouldBe` [(alice, Four ## Spades), (bob, Jack ## Diamonds), (charlie, Queen ## Hearts)]
        -- Check hands seem OK too
        length hands `shouldBe` 3
        hands `shouldBe` [(alice, emptyHand), (bob, emptyHand), (charlie, emptyHand)]


integrationSpec :: Spec
integrationSpec = describe "PlayGame" $
    it "plays a whole game out" $ do
      let dealer = RikikiDealingFor 3
          scorer = ProgressiveScoring {flatBonus = 10, penaltyFactor = -1}
          players = NonEmpty.fromList [alice, bob, charlie]
          deck = fullDeck
          gen = mkStdGen 0
      let (log, results) = evalRand (runGame dealer scorer players deck) gen

      log `shouldContain` "Round: #01"
      length results `shouldBe` 16 * 2 + 1
