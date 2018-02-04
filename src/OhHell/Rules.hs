module OhHell.Rules where

import Prelude ()
import ClassyPrelude

import OhHell.Core
import qualified Data.Set as Set
import qualified Data.List as List
import Game.Implement.Card.Standard (Suit,toSuit,PlayingCard,toRank,PlayingCard(PlayingCard))

-- | Rules (variations) governing scoring
-- Notably 1. Score formula if taken tricks missed bid
--         2. Score formula if taken tricks was exactly bid
class ScorerRules a where
  scoreForPlayerRound :: a -> RoundResult -> Score

data ProgressiveScoring = ProgressiveScoring {flatBonus :: Int, penaltyFactor :: Int}

instance ScorerRules ProgressiveScoring where
  scoreForPlayerRound sr (RoundResult bid taken)
   | bid == taken = flatBonus sr + bid * bid
   | otherwise    = penaltyFactor sr * abs (bid - taken)



-- | Rules (variations) governing playing
-- Notably: 1. Number of cards dealt per round
--          2. What constitutes a valid bid for a player on a round (e.g. bid-busting)
class DealerRules dr where
  -- | Number of players in this rule set
  numPlayers :: dr -> NumPlayers

  numRounds :: dr -> NumRounds

  -- | Number of cards to deal in the given round
  numCardsForRound :: dr -> RoundNum -> NumCards

  -- | Is bid-busting (last bidder can't allow a total bid equal to hand size) enabled?
  bidBusting :: dr -> Bool

  -- | What are all the valid bids given the previous bids
  validBids :: dr
            -> NumCards
            -> BidsFor p
            -> Set Bid
  validBids rules numCards bids
    | numCards <= 0 = Set.empty
    | bidBusting rules && List.length bids == (numPlayers rules - 1) = Set.difference allBids disallowed
    | otherwise = allBids
    where allBids = Set.fromList [0..numCards]
          disallowed = Set.singleton $ numCards - sum (snd <$> bids)

  -- | What are all the valid cards given previous cards and a hand
  -- TODO: support no-leading-with-trumps-until-broken
  validCards :: dr
             -> Trick p
             -> Hand
             -> Set PlayingCard
  validCards rules trickSoFar (Hand hand)
    | List.null trickSoFar = hand
    | List.null following  = hand
    | otherwise            = following
    where leadSuit  = (toSuit . snd . List.head) trickSoFar
          following = Set.filter ((==leadSuit) . toSuit) hand

-- | Rikiki-style dealing, for a given number of players
newtype RikikiDealing = RikikiDealingFor NumPlayers

instance DealerRules RikikiDealing where
  numPlayers (RikikiDealingFor n) = n

  numRounds dr = 2 * (51 `div` numPlayers dr) + 1

  numCardsForRound dr roundNum = (up ++ maxRoundSize : down ++ zeros) List.!! (roundNum - 1)
                    where maxRoundSize = 51 `div` numPlayers dr
                          up = [1..maxRoundSize - 1]
                          down = List.reverse up
                          zeros = List.repeat 0
  bidBusting dr = True


-- | Generate scores given some rules and some results
scoresFor :: (Ord p, ScorerRules r)
          => r
          -> ResultsFor p
          -> ScoresBy p
scoresFor rules results = unionsWith (+) roundScores
  where roundScores = fmap scoresFromResults results
        scoresFromResults = fmap (scoreForPlayerRound rules)

-- | Trick Winner
trickWinnerFor :: Maybe Suit -> Trick p -> p
trickWinnerFor trumps cards = recurse cards Nothing
    where recurse [] (Just (best, _)) = ownerOf best
          recurse ((p, card) : trick') Nothing = recurse trick' $ Just (card, toSuit card)
          recurse ((p, card) : trick') (Just (best, leadSuit)) = recurse trick' $ Just (higherCard trumps leadSuit card best, leadSuit)
          ownerOf card = fst $ List.head $ List.filter ((== card) . snd)  cards

higherCard :: Maybe Suit -> Suit -> PlayingCard -> PlayingCard -> PlayingCard
higherCard (Just trumps) leadSuit a@(PlayingCard ar as) b@(PlayingCard br bs)
  | as == bs            = if ar > br then a else b
  | as == trumps        = a
  | bs == trumps        = b
  | otherwise           = higherCard Nothing leadSuit a b
higherCard Nothing leadSuit a@(PlayingCard ar as) b@(PlayingCard br bs)
  | as == leadSuit && bs /= leadSuit = a
  | bs == leadSuit && as /= leadSuit = b
  | otherwise                        = if ar > br then a else b
