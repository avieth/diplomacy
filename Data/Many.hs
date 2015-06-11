{-|
Module      : Data.Many
Description : Lists of at least one element
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}

module Data.Many (

    Many(..)

  , makeMany
  , manyCons
  , manySnoc
  , manyAppend
  , mfirst
  , mlast
  , manyAny
  , manyToList

  , SplitMany(..)
  , splitMany

  , popMany

  ) where

import Prelude hiding (foldr)
import Data.Foldable

data Many t = One t | Many t (Many t)

instance Show t => Show (Many t) where
    show = show . manyToList

instance Functor Many where
    fmap f xs = case xs of
        One x -> One (f x)
        Many x xs -> Many (f x) (fmap f xs)

instance Foldable Many where
    foldr f b = foldr f b . manyToList

manyCons :: t -> Many t -> Many t
manyCons = Many

manySnoc :: Many t -> t -> Many t
manySnoc xs x = case xs of
    One x' -> Many x' (One x)
    Many x' xs' -> Many x' (manySnoc xs' x)

manyAppend :: Many t -> Many t -> Many t
manyAppend xs ys = case xs of
    One x -> Many x ys
    Many x xs' -> Many x (manyAppend xs' ys)

mfirst :: Many t -> t
mfirst many = case many of
    One x -> x
    Many x _ -> x

mlast :: Many t -> t
mlast many = case many of
    One x -> x
    Many _ rest -> mlast rest

manyAny :: (t -> Bool) -> Many t -> Bool
manyAny p xs = case xs of
    One x -> p x
    Many x xs' -> p x || manyAny p xs'

manyToList :: Many t -> [t]
manyToList xs = case xs of
    One x -> [x]
    Many x xs' -> x : manyToList xs'

makeMany :: t -> [t] -> Many t
makeMany x xs = case xs of
    [] -> One x
    x' : xs' -> Many x (makeMany x' xs')

data SplitMany t
    = SplitNotPresent -- Splitter was not present.
    | SplitEmpty -- Splitter was the only element.
    | SplitEnd (Many t) -- Splitter was at the end (left or right).
    | SplitMiddle (Many t) (Many t) -- Splitter was in the middle.
  deriving (Show)

splitMany :: Eq t => t -> Many t -> SplitMany t
splitMany x xs = case xs of
    One x' -> if x' == x then SplitEmpty else SplitNotPresent
    Many x' xs' -> if x' == x then SplitEnd xs' else splitManyRec x xs' (One x')
  where
    splitManyRec :: Eq t => t -> Many t -> Many t -> SplitMany t
    splitManyRec x xs left = case xs of
        One x' -> if x' == x then SplitEnd left else SplitNotPresent
        Many x' xs' -> if x' == x then SplitMiddle left xs' else splitManyRec x xs' (manySnoc left x')

popMany :: Many t -> (t, Maybe (Many t))
popMany xs = case xs of
    One x -> (x, Nothing)
    Many x xs -> (x, Just xs)
