{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Diplomacy.Dislodgement (

    Dislodgement

  , singleDislodgement
  , insertDislodgement
  , removeDislodgement
  , checkDislodgement
  , dislodged

  ) where

import qualified Data.Map as M
import           Data.Monoid
import           Data.Foldable

import           Diplomacy.ProvinceTargetMap
import           Diplomacy.Province

newtype Dislodgement a = Dislodgement {
    outDislodgement :: ProvinceTargetMap a
  } deriving (Show, Functor, Monoid, Foldable)

singleDislodgement :: ProvinceTarget -> a -> Dislodgement a
singleDislodgement pt = Dislodgement . singleProvinceTargetMap pt

checkDislodgement :: ProvinceTarget -> Dislodgement a -> Maybe a
checkDislodgement pt = checkProvinceTargetMap pt . outDislodgement

insertDislodgement :: ProvinceTarget -> a -> Dislodgement a -> Dislodgement a
insertDislodgement pt a = Dislodgement . insertProvinceTargetMap pt a . outDislodgement

removeDislodgement :: ProvinceTarget -> Dislodgement a -> Dislodgement a
removeDislodgement pt = Dislodgement . removeProvinceTargetMap pt . outDislodgement

dislodged :: Eq a => a -> ProvinceTarget -> Dislodgement a -> Bool
dislodged x pt occupation = maybe False (== x) (checkDislodgement pt occupation)
