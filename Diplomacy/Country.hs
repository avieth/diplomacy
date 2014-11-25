module Diplomacy.Country (

    Country(..)

  ) where

data Country
  = UnitedKingdom
  | France
  | Austria
  | Italy
  | Germany
  | Ottoman
  | Russia
    deriving (Eq, Ord, Enum, Bounded, Show)
