module Turn (

    Turn

  , firstTurn
  , nextTurn
  , asYear

  ) where

newtype Turn = Turn Int
  deriving (Eq, Ord)

instance Show Turn where
  show = show . asYear

firstTurn :: Turn
firstTurn = Turn 1

nextTurn :: Turn -> Turn
nextTurn (Turn i) = Turn (i+1)

asYear :: Turn -> Int
asYear (Turn n) = 1900 + n
