module Diplomacy.OrderSubject (

    OrderSubject(..)
  , orderSubjectUnit
  , orderSubjectTarget

  ) where

import Diplomacy.Unit
import Diplomacy.Province

-- | Subject of an order; the thing which is ordered.
--   We describe it just like it's described in the rules:
--     F London
--     A Berlin
--     etc.
data OrderSubject = OrderSubject Unit ProvinceTarget
  deriving (Eq, Ord)

orderSubjectUnit :: OrderSubject -> Unit
orderSubjectUnit (OrderSubject u _) = u

orderSubjectTarget :: OrderSubject -> ProvinceTarget
orderSubjectTarget (OrderSubject _ pt) = pt

instance Show OrderSubject where
  show (OrderSubject u pt) = show u ++ " " ++ show pt
