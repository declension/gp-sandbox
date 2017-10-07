module GameSpec where

import Test.Hspec
import OhHell

spec :: Spec
spec = basicSpec

alice = Player 1 "Alice"
bob = Player 2 "Bob"
charlie = Player 3 "Charlie"


basicSpec :: Spec
basicSpec = describe "The Game Engine" $ do

  it "constructs players properly" $ do
    playerName charlie `shouldBe` "Charlie"

  it "shows players nicely" $ do
    show alice `shouldContain` "Alice"
    show alice `shouldContain` "1"

  it "constructs HandResults " $ do
    let handResult = PlayerRoundResult 1 2
    handBid handResult `shouldBe` 1
    handTaken handResult `shouldBe` 2
