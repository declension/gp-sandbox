module GameSpec where

import Test.Hspec
import OhHell
import OhHell.Strategy (calculateBid, RandomBidder(RandomBidder))
import qualified Data.List.NonEmpty as NonEmpty (fromList)
import qualified Data.Set as Set
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

alice = Player 1 "Alice"
bob = Player 2 "Bob"
charlie = Player 3 "Charlie"
prr = PlayerRoundResult


basicSpec :: Spec
basicSpec =
  describe "The game engine" $ do

    it "constructs players properly" $ do
      playerName charlie `shouldBe` "Charlie"

    it "shows players nicely" $ do
      show alice `shouldContain` "Alice"
      show alice `shouldContain` "1"

    it "constructs HandResults " $ do
      let handResult = PlayerRoundResult 1 2
      handBid handResult `shouldBe` 1
      handTaken handResult `shouldBe` 2

scoringSpec :: Spec
scoringSpec = describe "Scoring" $ do

    it "sums scores correctly for Progressive Scoring" $ do
      -- A combination of scores that should show everything
      let results = NonEmpty.fromList [(alice,   [prr 0 0, prr 1 0, prr 1 1, prr 3 3]),
                                       (bob,     [prr 0 1, prr 0 1, prr 1 0, prr 0 0]),
                                       (charlie, [prr 0 1, prr 0 0, prr 1 1, prr 0 2])]
      let rules = ProgressiveScoring 10 (-1)
      scoresFor rules results `shouldBe` NonEmpty.fromList [(alice, 39), (bob, 7), (charlie, 18)]

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
      validBids dealer 4 [(alice, 0), (bob, 1)] `shouldBe` Set.fromList [0, 1, 2, 4]

biddingSpec :: Spec
biddingSpec = describe "Dealing and bidding for a round" $ do
    it "should deplete the deck" $ do
      let dealer = RikikiDealingFor 3
      let players = [alice, bob, charlie]
      let g = mkStdGen 0
      let deck = fullDeck
      let (results, deck') = evalRand (bidOnRound dealer 2 deck players) g
--      results `shouldBe` [(alice, 2), (bob, 2), (charlie, 0)]
      (len deck') `shouldBe` (52 - 6)


strategySpec :: Spec
strategySpec = describe "RandomStrategy" $
    it "should respect dealer" $ do
      let dealer = RikikiDealingFor 3
      let player = alice
      let numCards = 4
      let cards = NonEmpty.fromList $ take numCards fullDeck
      bid <- calculateBid RandomBidder dealer player [(bob, 2), (charlie, 1)] (Hand cards)
      bid `shouldSatisfy` (<= numCards)
      bid `shouldSatisfy` (>= 0)
      bid `shouldSatisfy` (/= 1)

