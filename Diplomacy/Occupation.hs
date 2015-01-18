{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Diplomacy.Occupation (

    Occupation

  , singleOccupation
  , insertOccupation
  , removeOccupation
  , checkOccupation
  , occupies

  ) where

import qualified Data.Map as M
import           Data.Monoid
import           Data.Foldable

import           Diplomacy.ProvinceTargetMap
import           Diplomacy.Province

newtype Occupation a = Occupation {
    outOccupation :: ProvinceTargetMap a
  } deriving (Show, Functor, Monoid, Foldable)

singleOccupation :: ProvinceTarget -> a -> Occupation a
singleOccupation pt = Occupation . singleProvinceTargetMap pt

checkOccupation :: ProvinceTarget -> Occupation a -> Maybe a
checkOccupation pt = checkProvinceTargetMap pt . outOccupation

insertOccupation :: ProvinceTarget -> a -> Occupation a -> Occupation a
insertOccupation pt x = Occupation . insertProvinceTargetMap pt x . outOccupation

removeOccupation :: ProvinceTarget -> Occupation a -> Occupation a
removeOccupation pt = Occupation . removeProvinceTargetMap pt . outOccupation

occupies :: Eq a => a -> ProvinceTarget -> Occupation a -> Bool
occupies x pt occupation = maybe False (== x) (checkOccupation pt occupation)
