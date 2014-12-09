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
  , ValidOrder

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

import Diplomacy.Order

-- Each turn is divided into 5 phases. We give a type for each of them.

data Spring
data SpringRetreat
data Autumn
data AutumnRetreat
data Winter

-- It will be useful to have a type-level cyclic order of the above types.

type family NextPhase x where
  NextPhase Spring = SpringRetreat
  NextPhase SpringRetreat = Autumn
  NextPhase Autumn = AutumnRetreat
  NextPhase AutumnRetreat = Winter
  NextPhase Winter = Spring

-- | Unfortunate name: this has nothing to do with order as in mathematics, but
--   only to do with Order as in our datatype for Diplomacy orders.
--   It indicates which kind of OrderTarget is valid for a given Phase.
type family ValidOrder x where
  ValidOrder Spring = Typical
  ValidOrder SpringRetreat = Retreat
  ValidOrder Autumn = Typical
  ValidOrder AutumnRetreat = Retreat
  ValidOrder Winter = Adjust

-- | A phase is parameterized by a phase type.
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
