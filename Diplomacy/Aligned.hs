module Aligned (

    Aligned
  , align
  , unalign
  , alignedCountry

  ) where

import Diplomacy.Country

data Aligned a = Aligned a Country
  deriving (Eq, Ord)

align :: a -> Country -> Aligned a
align = Aligned

unalign :: Aligned a -> a
unalign (Aligned x _) = x

alignedCountry :: Aligned a -> Country
alignedCountry (Aligned _ c) = c

instance Functor Aligned where
  fmap f (Aligned x c) = Aligned (f x) c

instance Show a => Show (Aligned a) where
  show (Aligned x c) = show c ++ " : " ++ show x
