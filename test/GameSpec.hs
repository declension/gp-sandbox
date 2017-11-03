module GameSpec where

import Prelude ()
import ClassyPrelude

import OhHell.Core
import OhHell.Rules
import OhHell.Player
import OhHell.Strategies (RandomBidder(RandomBidder))


import Test.Hspec
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty (fromList)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad.State (execState)
import Control.Monad.Random (mkStdGen,evalRandT,evalRand)
import OhHell.Game (bidOnRound)
import Game.Implement.Card (fullDeck)

spec :: Spec
spec = do
  basicSpec
  dealingSpec
  scoringSpec
  biddingSpec
  strategySpec

alice = RandomBidder "Alice"
bob = RandomBidder "Bob"
charlie = RandomBidder "Charlie"
prr = PlayerRoundResult
pidOf = getPlayerId

basicSpec :: Spec
basicSpec =
  describe "The game engine" $ do

    it "constructs players properly" $ do
      getPlayerName charlie `shouldBe` "Charlie"

    it "shows players nicely" $ do
      show alice `shouldContain` "Alice"

    it "constructs HandResults " $ do
      let handResult = PlayerRoundResult 1 2
      handBid handResult `shouldBe` 1
      handTaken handResult `shouldBe` 2

scoringSpec :: Spec
scoringSpec = describe "Scoring" $ do

    it "sums scores correctly for Progressive Scoring" $ do
      -- A combination of scores that should show everything
      let results = [ Map.fromList [(pidOf alice, prr 0 0), (pidOf bob, prr 0 1), (pidOf charlie, prr 0 0)]
                    , Map.fromList [(pidOf alice, prr 1 0), (pidOf bob, prr 0 0), (pidOf charlie, prr 1 1)]
                    , Map.fromList [(pidOf alice, prr 3 3), (pidOf bob, prr 0 0), (pidOf charlie, prr 1 0)]
                    ]

      let rules = ProgressiveScoring 10 (-1)
      scoresFor rules results `shouldBe` Map.fromList [(pidOf alice, 28),
                                                       (pidOf bob, 19),
                                                       (pidOf charlie, 20)]

dealingSpec :: Spec
dealingSpec = describe "Dealing" $ do

    it "deals correctly for Rikiki Rules" $ do
      let dealer = RikikiDealingFor 5
      numCardsForRound dealer 1 `shouldBe` 1
      numCardsForRound dealer 2 `shouldBe` 2
      numCardsForRound dealer 5 `shouldBe` 5
      numCardsForRound dealer 10 `shouldBe` 10
      numCardsForRound dealer 11 `shouldBe` 9

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
      validBids dealer 4 [(pidOf alice, 0), (pidOf bob, 1)] `shouldBe` Set.fromList [0, 1, 2, 4]


biddingSpec :: Spec
biddingSpec = describe "Dealing and bidding for a round" $ do
    it "should return the right number of results" $ do
      let dealer = RikikiDealingFor 3
      let playerHands = NonEmpty.fromList
                        [(alice, handOf []),
                         (bob, handOf []),
                         (charlie, handOf [])] :: NonEmpty (RandomBidder, Hand)
      let g = mkStdGen 0
      let results = evalRand (bidOnRound dealer 2 Nothing playerHands) g
      List.length results `shouldBe` 3


strategySpec :: Spec
strategySpec = describe "RandomStrategy" $
    it "should respect dealer" $ do
      let dealer = RikikiDealingFor 3
      let player = alice
      let numCards = 4
      let cards = NonEmpty.fromList $ take numCards fullDeck
      let trumps = Nothing
      bid <- chooseBid player dealer trumps [(pidOf bob, 2), (pidOf charlie, 1)] (Hand cards)
      bid `shouldSatisfy` (<= numCards)
      bid `shouldSatisfy` (>= 0)
      bid `shouldSatisfy` (/= 1)

