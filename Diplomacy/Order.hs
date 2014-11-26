module Diplomacy.Order (

    Order(..)

  ) where

import Diplomacy.Unit
import Diplomacy.Province

-- | This type may seem too big; it contains a lot of invalid orders. But this
--   is intentional. The rules of diplomacy admit invalid orders; they are
--   discarded! It's up to the user to form the orders properly.
data Order
  = Hold Unit ProvinceTarget
  | Move Unit ProvinceTarget ProvinceTarget
  | Support Unit ProvinceTarget Unit ProvinceTarget ProvinceTarget
  -- ^ Support second unit into province with the first unit
  | Convoy Unit ProvinceTarget Unit ProvinceTarget ProvinceTarget
    deriving (Eq, Ord)
