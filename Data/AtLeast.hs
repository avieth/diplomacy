{-|
Module      : Data.AtLeast
Description : Lists of at least n elements.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
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

data AtLeast (n :: Nat) (t :: *) = AtLeast (Vect t n) [t]

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
toList (AtLeast vs xs) = xs ++ vectToList vs

head :: AtLeast One t -> t
head (AtLeast vs xs) = case (vs, xs) of
    (VCons x _, []) -> x
    (_, x : _) -> x

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

{-

-- | A list of at least @n@ elements.
data AtLeast (n :: Nat) (t :: *) where
    ALNil :: AtLeast Z t
    ALCons :: t -> AtLeast n t -> AtLeast (S n) t
    ALExtend :: t -> AtLeast n t -> AtLeast n t

infixr 8 ::>
infixr 8 ::+

pattern x ::> y = ALCons x y
pattern x ::+ y = ALExtend x y

appendList :: AtLeast n t -> [t] -> AtLeast n t
appendList atLeast ts = case atLeast of
    ALNil -> fromList ts
    ALCons x rest -> ALCons x (appendList rest ts)
    ALExtend x rest -> ALExtend x (appendList rest ts)

fromList :: [t] -> AtLeast Z t
fromList xs = case xs of
    [] -> ALNil
    (x : xs) -> ALExtend x (fromList xs)

toList :: AtLeast n t -> [t]
toList atLeast = case atLeast of
    ALNil -> []
    ALCons x rest -> x : toList rest
    ALExtend x rest -> x : toList rest

head :: AtLeast One t -> t
head atLeast = case atLeast of
    ALCons x _ -> x
    ALExtend x _ -> x

newtype Weaken t n = Weaken {
    unWeaken :: AtLeast n t
  }

weaken1 :: AtLeast (S n) t -> AtLeast n t
weaken1 atLeast = case atLeast of
    ALCons x rest -> ALExtend x rest
    ALExtend x rest -> ALExtend x (weaken1 rest)

weaken :: forall n m t . LTE n m => AtLeast m t -> AtLeast n t
weaken = unWeaken . lteRecursion recurse . Weaken
  where
    recurse :: forall k . LTE n k => Weaken t (S k) -> Weaken t k
    recurse (Weaken atLeast) = Weaken (weaken1 atLeast)

maxima :: (t -> t -> Ordering) -> AtLeast One t -> AtLeast One t
maxima comparator atLeast = case atLeast of
    ALCons x rest -> maxima' comparator (ALCons x ALNil) (toList rest)
    ALExtend x rest -> maxima' comparator (ALCons x ALNil) (toList rest)
  where
    maxima' :: (t -> t -> Ordering) -> AtLeast One t -> [t] -> AtLeast One t
    maxima' comparator acc rest = case rest of
        [] -> acc
        (x : rest) -> case comparator (head acc) x of
            GT -> maxima' comparator acc rest
            EQ -> maxima' comparator (ALExtend x acc) rest
            LT -> maxima' comparator (ALCons x ALNil) rest
-}
