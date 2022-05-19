{-|
Module      : Data.AtLeast
Description : Lists of at least n elements.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.AtLeast (

    AtLeast(..)

  , fromList
  , toList
  , appendList

  , weaken
  , maxima

  , head

  ) where

import Prelude hiding (head)
import Data.List ((\\))
import Data.Ord
import Data.TypeNat.Nat
import Data.TypeNat.Vect

data AtLeast (n :: Nat) (t :: *) = AtLeast (Vect n t) [t]

-- Equality ignores order of elements.
instance Eq t => Eq (AtLeast n t) where
    (==) xs ys = case (toList xs) \\ (toList ys) of
        [] -> True
        _ -> False

deriving instance Show t => Show (AtLeast n t)

appendList :: AtLeast n t -> [t] -> AtLeast n t
appendList (AtLeast vect rest) xs = AtLeast vect (xs ++ rest)

fromList :: [t] -> AtLeast Z t
fromList xs = AtLeast VNil xs

toList :: AtLeast n t -> [t]
toList (AtLeast vs xs) = vectToList vs ++ xs

head :: AtLeast One t -> t
head (AtLeast vs xs) = case (vs, xs) of
    (VCons x _, _) -> x

newtype Weaken t n = Weaken {
    unWeaken :: AtLeast n t
  }

weaken1 :: AtLeast (S n) t -> AtLeast n t
weaken1 (AtLeast vs xs) = case vs of
    VCons x rest -> AtLeast rest (x : xs)

weaken :: forall n m t . LTE n m => AtLeast m t -> AtLeast n t
weaken = unWeaken . lteRecursion recurse . Weaken
  where
    recurse :: forall k . LTE n k => Weaken t (S k) -> Weaken t k
    recurse (Weaken atLeast) = Weaken (weaken1 atLeast)

maxima :: (t -> t -> Ordering) -> AtLeast One t -> AtLeast One t
maxima comparator (AtLeast vs xs) = case vs of
    VCons x rest -> maxima' comparator (AtLeast (VCons x VNil) []) (vectToList rest ++ xs)
  where
    maxima' :: (t -> t -> Ordering) -> AtLeast One t -> [t] -> AtLeast One t
    maxima' comparator acc rest = case rest of
        [] -> acc
        (x : rest) -> case comparator (head acc) x of
            GT -> maxima' comparator acc rest
            EQ -> maxima' comparator (appendList acc [x]) rest
            LT -> maxima' comparator (AtLeast (VCons x VNil) []) rest
