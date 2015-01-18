{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Diplomacy.ProvinceTargetMap (

    ProvinceTargetMap

  , singleProvinceTargetMap
  , insertProvinceTargetMap
  , removeProvinceTargetMap
  , checkProvinceTargetMap

  ) where

import qualified Data.Map as M
import           Data.Monoid
import           Data.Foldable

import           Diplomacy.Province

newtype ProvinceTargetMap a = ProvinceTargetMap {
    outProvinceTargetMap :: M.Map ProvinceTarget a
  } deriving (Show, Functor, Monoid, Foldable)

singleProvinceTargetMap :: ProvinceTarget -> a -> ProvinceTargetMap a
singleProvinceTargetMap pt a = ProvinceTargetMap (M.singleton pt a)

insertProvinceTargetMap :: ProvinceTarget -> a -> ProvinceTargetMap a -> ProvinceTargetMap a
insertProvinceTargetMap pt a = ProvinceTargetMap . M.insert pt a . outProvinceTargetMap

removeProvinceTargetMap :: ProvinceTarget -> ProvinceTargetMap a -> ProvinceTargetMap a
removeProvinceTargetMap pt = ProvinceTargetMap . M.delete pt . outProvinceTargetMap

checkProvinceTargetMap :: ProvinceTarget -> ProvinceTargetMap a -> Maybe a
checkProvinceTargetMap pt = M.lookup pt . outProvinceTargetMap
