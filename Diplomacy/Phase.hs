{-# LANGUAGE GADTs, TypeFamilies #-}

module Diplomacy.Phase (

  -- Types
    Spring
  , SpringRetreat
  , Autumn
  , AutumnRetreat
  , Winter
  , Phase
  , NextPhase
  , PhaseType
  , Typical
  , Retreat
  , Adjust

  -- Injections into Phase
  , springPhase
  , springRetreatPhase
  , autumnPhase
  , autumnRetreatPhase
  , winterPhase

  -- Indicators on Phase
  , isSpring
  , isSpringRetreat
  , isAutumn
  , isAutumnRetreat
  , isWinter

  , nextPhase

  ) where

-- Phases
-- Each turn is divided into 5 phases. We give a type for each of them.

data Spring
data SpringRetreat
data Autumn
data AutumnRetreat
data Winter

-- It will be useful to have a type-level cyclic order of the above types.
-- As far as I can tell, it's not possible to define an *injective* type
-- level cyclic function, since we cannot use the image of a data instance
-- as the preimage of another.
type family NextPhase x where
  NextPhase Spring = SpringRetreat
  NextPhase SpringRetreat = Autumn
  NextPhase Autumn = AutumnRetreat
  NextPhase AutumnRetreat = Winter
  NextPhase Winter = Spring

-- Phase types
-- Each phase type has an associated phase type

data Typical
data Retreat
data Adjust

type family PhaseType x where
  PhaseType Spring = Typical
  PhaseType SpringRetreat = Retreat
  PhaseType Autumn = Typical
  PhaseType AutumnRetreat = Retreat
  PhaseType Winter = Adjust

-- | A Phase value is parameterized by a phase type.
data Phase a where
  PSpring :: Phase Spring
  PSpringRetreat :: Phase SpringRetreat
  PAutumn :: Phase Autumn
  PAutumnRetreat :: Phase AutumnRetreat
  PWinter :: Phase Winter

instance Show (Phase a) where
  show PSpring = "Spring"
  show PSpringRetreat = "Spring Retreat"
  show PAutumn = "Autumn"
  show PAutumnRetreat = "Autumn Retreat"
  show PWinter = "Winter"

instance Eq (Phase a) where
  PSpring == PSpring = True
  PSpringRetreat == PSpringRetreat = True
  PAutumn == PAutumn = True
  PAutumnRetreat == PAutumnRetreat = True
  PWinter == PWinter = True
  _ == _ = False

instance Ord (Phase a) where
  PSpring <= _ = True
  _ <= PSpring = False
  PSpringRetreat <= _ = True
  _ <= PSpringRetreat = False
  PAutumn <= _ = True
  _ <= PAutumn = False
  PAutumnRetreat <= _ = True
  _ <= PAutumnRetreat = False
  PWinter <= PWinter = True

-- | A cyclic ordering of phases.
nextPhase :: Phase a -> Phase (NextPhase a)
nextPhase PSpring = PSpringRetreat
nextPhase PSpringRetreat = PAutumn
nextPhase PAutumn = PAutumnRetreat
nextPhase PAutumnRetreat = PWinter
nextPhase PWinter = PSpring

springPhase = PSpring

springRetreatPhase = PSpringRetreat

autumnPhase = PAutumn

autumnRetreatPhase = PAutumnRetreat

winterPhase = PWinter

isSpring PSpring = True
isSpring _ = False

isSpringRetreat PSpringRetreat = True
isSpringRetreat _ = False

isAutumn PAutumn = True
isAutumn _ = False

isAutumnRetreat PAutumnRetreat = True
isAutumnRetreat _ = False

isWinter PWinter = True
isWinter _ = False
