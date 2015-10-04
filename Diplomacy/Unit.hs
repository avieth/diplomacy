{-|
Module      : Diplomacy.Unit
Description : Definition of units (armies and fleets)
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Diplomacy.Unit (

    Unit(..)

  , parseUnit
  , printUnit

  ) where

import Control.Applicative
import Data.String (IsString)
import Text.Parsec hiding ((<|>))
import Text.Parsec.Text

data Unit where
    Army :: Unit
    Fleet :: Unit

deriving instance Eq Unit
deriving instance Ord Unit
deriving instance Show Unit
deriving instance Enum Unit
deriving instance Bounded Unit

parseUnit :: Parser Unit
parseUnit = parseFleet <|> parseArmy
  where
    parseFleet :: Parser Unit
    parseFleet = (char 'F' <|> char 'f') *> pure Fleet
    parseArmy :: Parser Unit
    parseArmy = (char 'A' <|> char 'a') *> pure Army

printUnit :: IsString a => Unit -> a
printUnit unit = case unit of
    Army -> "A"
    Fleet -> "F"
