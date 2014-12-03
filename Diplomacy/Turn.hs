module Diplomacy.Turn (

    Turn

  , firstTurn
  , nextTurn
  , asYear

  ) where

newtype Turn = Turn Int
  deriving (Eq, Ord, Show)

firstTurn :: Turn
firstTurn = Turn 1

nextTurn :: Turn -> Turn
nextTurn (Turn i) = Turn (i+1)

-- | Always > base. Proof?
--   Suffices to show that n > 0
--   If we lift > to Turn then
--     firstTurn > 0
--     nextTurn t > t
--   Any turn is either firstTurn or nextTurn t
asYear :: Int -> Turn -> Int
asYear base (Turn n) = base + n
