module Phase (

    Phase

  , firstPhase
  , nextPhase

  , isSpring
  , isAutumn
  , isWinter

  ) where

data Phase
  = Spring
  | Autumn
  | Winter
    deriving (Eq, Ord, Show)

firstPhase :: Phase
firstPhase = Spring

nextPhase :: Phase -> Phase
nextPhase Spring = Autumn
nextPhase Autumn = Winter
nextPhase Winter = Spring

isSpring :: Phase -> Bool
isSpring (Spring _) = True
isSpring _ = False

isAutumn :: Phase -> Bool
isAutumn (Autumn _) = True
isAutumn _ = False

isWinter :: Phase -> Bool
isWinter (Winter) = True
isWinter _ = False
