module GameSpec where

import Test.Hspec
import OhHell
import qualified Data.List.NonEmpty as NonEmpty (fromList)

spec :: Spec
spec = basicSpec

alice = Player 1 "Alice"
bob = Player 2 "Bob"
charlie = Player 3 "Charlie"
prr = PlayerRoundResult


basicSpec :: Spec
basicSpec = describe "The game engine" $ do

  it "constructs players properly" $ do
    playerName charlie `shouldBe` "Charlie"

  it "shows players nicely" $ do
    show alice `shouldContain` "Alice"
    show alice `shouldContain` "1"

  it "constructs HandResults " $ do
    let handResult = PlayerRoundResult 1 2
    handBid handResult `shouldBe` 1
    handTaken handResult `shouldBe` 2

  it "sums scores correctly for Progressive Scoring" $ do
    -- A combination of scores that should show everything
    let results = NonEmpty.fromList [(alice,   [prr 0 0, prr 1 0, prr 1 1, prr 3 3]),
                                     (bob,     [prr 0 1, prr 0 1, prr 1 0, prr 0 0]),
                                     (charlie, [prr 0 1, prr 0 0, prr 1 1, prr 0 2])]
    let rules = ProgressiveScoring 10 (-1)
    scoresFor rules results `shouldBe` NonEmpty.fromList [(alice, 39), (bob, 7), (charlie, 18)]
