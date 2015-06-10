{-|
Module      : Data.MayFail
Description : 
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}

module Data.MayFail (

    MayFail(..)

  , fails
  , passes

  , hoist

  ) where

import Control.Applicative
import Control.Monad
import Data.Functor.Compose
import Data.Functor.Identity

newtype MayFail e f t = MayFail {
    runMayFail :: Compose (Either e) f t
  } deriving (Functor, Applicative)

instance Alternative f => Alternative (MayFail e f) where
    empty = MayFail . Compose . Right $ empty
    x <|> y = MayFail . Compose $ case (x', y') of
        (Left e, _) -> Left e
        (_, Left e) -> Left e
        (Right x, Right y) -> Right (x <|> y)
      where
        x' = getCompose . runMayFail $ x
        y' = getCompose . runMayFail $ y

class CommutesWithEither f where
    commuteEither :: f (Either e a) -> Either e (f a)

instance CommutesWithEither Identity where
    commuteEither = fmap Identity . runIdentity

instance (Functor f, Monad f, CommutesWithEither f) => Monad (MayFail e f) where
    return = MayFail . Compose . return . return
    x >>= k = MayFail . Compose $ case getCompose . runMayFail $ x of
        Left e -> Left e
        Right fterm -> case commuteEither . fmap (getCompose . runMayFail . k) $ fterm of
            Left e' -> Left e'
            Right x -> Right (join x)

fails :: e -> MayFail e f t
fails = MayFail . Compose . Left

passes :: f t -> MayFail e f t
passes = MayFail . Compose . Right

hoist :: (f t -> g s) -> MayFail e f t -> MayFail e g s
hoist trans (MayFail (Compose x)) = MayFail (Compose (fmap trans x))
