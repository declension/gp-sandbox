module OhHell.Pretty where

import           ClassyPrelude hiding (intercalate, toList)
import qualified Data.Set as Set
import qualified Data.Map as Map
import           Data.Set (Set)
import           Data.Map (Map)
import           Game.Implement.Card.Standard
import           Data.List (intercalate)
import           Data.List.NonEmpty

class Pretty a where
    prettify :: a -> String

instance (Pretty a) => Pretty (Set a) where
    prettify set = "{" <> intercalate ", " list <> "}"
        where list = prettify <$> Set.toList set

instance (Pretty a) => Pretty [a] where
    prettify l = "[" <> intercalate ", " (prettify <$> l) <> "]"

instance Pretty Int where
    prettify = show

instance Pretty Char where
    prettify = show

instance (Pretty k, Pretty v) => Pretty (Map k v) where
    prettify m = "{" <> intercalate ", " list <> "}"
        where list = prettifyOne <$> Map.toList m
              prettifyOne (k, v) = prettify k <> ": " <> prettify v

instance (Pretty a) => Pretty (NonEmpty a) where
    prettify ne = prettify (toList ne)

instance (Pretty a, Pretty b) => Pretty (a, b) where
    prettify (a, b) = "(" <> prettify a <> ", " <> prettify b <> ")"

instance (Pretty a) => Pretty (Maybe a) where
    prettify Nothing   = "None"
    prettify (Just x)  = prettify x

instance Pretty PlayingCard where
    prettify (PlayingCard r s) = prettify r <> prettify s

instance Pretty Suit where
    prettify s
        | s == Clubs    = "♣"
        | s == Diamonds = "♦"
        | s == Hearts   = "♥"
        | s == Spades   = "♠"


instance Pretty Rank where
    prettify s
        | s == Ace      = "A"
        | s == Jack     = "J"
        | s == Queen    = "Q"
        | s == King     = "K"
        | otherwise     = show $ succ $ fromEnum s

