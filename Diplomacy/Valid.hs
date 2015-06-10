{-|
Module      : Diplomacy.Valid
Description :
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Diplomacy.Valid (

    Valid(..)

  ) where

newtype Valid t = Valid {
    outValid :: t
  } deriving (Eq, Ord, Show, Functor)
