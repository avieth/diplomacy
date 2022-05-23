{-|
Module      : Diplomacy.Order
Description : Definition of an order
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Diplomacy.Order (

    Order(..)

  , SomeOrder(..)
  , move
  , hold
  , support
  , convoy
  , surrender
  , retreat
  , build
  , disband
  , continue

  , orderSubject
  , orderObject

  , isHold
  , movingFrom
  , movingTo
  , supportsOrder

  , printSomeOrder
  , parseSomeOrder
  , printSubject
  , parseSubject
  , printObject
  , parseObject

  ) where

import Data.Coerce (coerce)
import Diplomacy.GreatPower
import Diplomacy.Aligned
import Diplomacy.Phase
import Diplomacy.Subject
import Diplomacy.OrderType
import Diplomacy.OrderObject
import Diplomacy.Province
import Diplomacy.Unit
import Data.Text as T
import Text.Parsec
import Text.Parsec.Text

newtype Order (phase :: Phase) (order :: OrderType) = Order {
    outOrder :: (Subject, OrderObject phase order)
  } deriving (Eq, Ord, Show)

coerce' :: Order phase order -> (Subject, OrderObject phase order)
coerce' = coerce

orderSubject :: Order phase order -> Subject
orderSubject = fst . coerce'

orderObject :: Order phase order -> OrderObject phase order
orderObject = snd . coerce'

data SomeOrder phase where
    SomeOrder :: Order phase order -> SomeOrder phase

instance Eq (SomeOrder phase) where
    SomeOrder o1 == SomeOrder o2 = case (orderObject o1, orderObject o2) of
        (MoveObject _, MoveObject _) -> o1 == o2
        (SupportObject _ _, SupportObject _ _) -> o1 == o2
        (ConvoyObject _ _, ConvoyObject _ _) -> o1 == o2
        (SurrenderObject, SurrenderObject) -> o1 == o2
        (WithdrawObject _, WithdrawObject _) -> o1 == o2
        (DisbandObject, DisbandObject) -> o1 == o2
        (BuildObject, BuildObject) -> o1 == o2
        (ContinueObject, ContinueObject) -> o1 == o2
        _ -> False

instance Ord (SomeOrder phase) where
    SomeOrder o1 `compare` SomeOrder o2 = show o1 `compare` show o2

deriving instance Show (SomeOrder phase)

move :: Unit -> ProvinceTarget -> ProvinceTarget -> SomeOrder Typical
move unit from target = SomeOrder (Order ((unit, from), MoveObject target))

hold :: Unit -> ProvinceTarget -> SomeOrder Typical
hold unit at = move unit at at

support :: Unit -> ProvinceTarget -> Unit -> ProvinceTarget -> ProvinceTarget -> SomeOrder Typical
support unit at unit' at' target = SomeOrder (Order ((unit, at), SupportObject (unit', at') target))

convoy :: Unit -> ProvinceTarget -> Unit -> ProvinceTarget -> ProvinceTarget -> SomeOrder Typical
convoy unit at unit' at' target = SomeOrder (Order ((unit, at), ConvoyObject (unit', at') target))

surrender :: Unit -> ProvinceTarget -> SomeOrder Retreat
surrender unit at = SomeOrder (Order ((unit, at), SurrenderObject))

retreat :: Unit -> ProvinceTarget -> ProvinceTarget -> SomeOrder Retreat
retreat unit at target = SomeOrder (Order ((unit, at), WithdrawObject target))

build :: Unit -> ProvinceTarget -> SomeOrder Adjust
build unit at = SomeOrder (Order ((unit, at), BuildObject))

disband :: Unit -> ProvinceTarget -> SomeOrder Adjust
disband unit at = SomeOrder (Order ((unit, at), DisbandObject))

continue :: Unit -> ProvinceTarget -> SomeOrder Adjust
continue unit at = SomeOrder (Order ((unit, at), ContinueObject))

isHold :: Order Typical Move -> Bool
isHold order = from == to
  where
    to = moveTarget . orderObject $ order
    from = subjectProvinceTarget . orderSubject $ order

movingFrom :: Order Typical Move -> ProvinceTarget
movingFrom = subjectProvinceTarget . orderSubject

movingTo :: Order Typical Move -> ProvinceTarget
movingTo = moveTarget . orderObject

supportsOrder :: OrderObject Typical Support -> SomeOrder Typical -> Bool
supportsOrder supportOrderObject (SomeOrder order) =
       supportedSubject supportOrderObject == orderSubject order
    && supportTarget supportOrderObject == orderDestination order
  where
    orderDestination :: Order Typical order -> ProvinceTarget
    orderDestination order = case orderObject order of
        MoveObject pt -> pt
        SupportObject _ _ -> subjectProvinceTarget (orderSubject order)

-- |
-- = Printing/parsing
--
-- Aims to be the typical board-game text representation of an order.

printSomeOrder :: SomeOrder phase -> T.Text
printSomeOrder (SomeOrder order) = T.concat [
      printSubject (orderSubject order)
    , " "
    , printObject (SomeOrderObject (orderObject order))
    ]

parseSomeOrder :: IsPhase phase -> Parser (SomeOrder phase)
parseSomeOrder IsTypicalPhase = parseSomeOrderTypical
parseSomeOrder IsRetreatPhase = parseSomeOrderRetreat
parseSomeOrder IsAdjustPhase = parseSomeOrderAdjust

parseSomeOrderTypical :: Parser (SomeOrder Typical)
parseSomeOrderTypical = do
    subject <- parseSubject
    spaces
    SomeOrderObject object <- parseObjectTypical
    return $ SomeOrder (Order (subject, object))

parseSomeOrderRetreat :: Parser (SomeOrder Retreat)
parseSomeOrderRetreat = do
    subject <- parseSubject
    spaces
    SomeOrderObject object <- parseObjectRetreat
    return $ SomeOrder (Order (subject, object))

parseSomeOrderAdjust :: Parser (SomeOrder Adjust)
parseSomeOrderAdjust = do
    subject <- parseSubject
    spaces
    SomeOrderObject object <- parseObjectAdjust
    return $ SomeOrder (Order (subject, object))

printSubject :: Subject -> T.Text
printSubject (unit, pt) = T.concat [
      printUnit unit
    , " "
    , printProvinceTarget pt
    ]

parseSubject :: Parser Subject
parseSubject = do
    unit <- parseUnit
    spaces
    pt <- parseProvinceTarget
    return (unit, pt)

printObject :: SomeOrderObject phase -> T.Text
printObject (SomeOrderObject object) = case object of
    MoveObject _ -> printMove object
    SupportObject _ _ -> printSupport object
    ConvoyObject _ _ -> printConvoy object
    SurrenderObject -> printSurrender object
    WithdrawObject _ -> printWithdraw object
    DisbandObject -> printDisband object
    BuildObject -> printBuild object
    ContinueObject -> printContinue object

parseObject :: IsPhase phase -> Parser (SomeOrderObject phase)
parseObject IsTypicalPhase = parseObjectTypical
parseObject IsRetreatPhase = parseObjectRetreat
parseObject IsAdjustPhase = parseObjectAdjust

parseObjectTypical :: Parser (SomeOrderObject Typical)
parseObjectTypical =
        (SomeOrderObject <$> try parseMove)
    <|> (SomeOrderObject <$> try parseSupport)
    <|> (SomeOrderObject <$> try parseConvoy)

parseObjectRetreat :: Parser (SomeOrderObject Retreat)
parseObjectRetreat =
        (SomeOrderObject <$> try parseSurrender)
    <|> (SomeOrderObject <$> try parseWithdraw)

parseObjectAdjust :: Parser (SomeOrderObject Adjust)
parseObjectAdjust =
        (SomeOrderObject <$> try parseDisband)
    <|> (SomeOrderObject <$> try parseBuild)
    <|> (SomeOrderObject <$> try parseContinue)

printMove :: OrderObject Typical Move -> T.Text
printMove (MoveObject pt) = T.concat ["- ", printProvinceTarget pt]

parseMove :: Parser (OrderObject Typical Move)
parseMove = do
    char '-'
    spaces
    pt <- parseProvinceTarget
    return $ MoveObject pt

printSupport :: OrderObject Typical Support -> T.Text
printSupport (SupportObject subj pt) =
  if subjectProvinceTarget subj == pt
  then T.concat ["S ", printSubject subj]
  else T.concat ["S ", printSubject subj, " - ", printProvinceTarget pt]

parseSupport :: Parser (OrderObject Typical Support)
parseSupport = do
    (char 'S' <|> char 's')
    spaces
    subject <- parseSubject
    target <- Text.Parsec.option (subjectProvinceTarget subject) (try rest)
    return $ SupportObject subject target
  where
    rest = do
        spaces
        char '-'
        spaces
        parseProvinceTarget

printConvoy :: OrderObject Typical Convoy -> T.Text
printConvoy (ConvoyObject subj pt) = T.concat ["C ", printSubject subj, " - ", printProvinceTarget pt]

parseConvoy :: Parser (OrderObject Typical Convoy)
parseConvoy = do
    (char 'C' <|> char 'c')
    spaces
    subject <- parseSubject
    spaces
    char '-'
    spaces
    target <- parseProvinceTarget
    return $ ConvoyObject subject target

printSurrender :: OrderObject Retreat Surrender -> T.Text
printSurrender SurrenderObject = "Surrender"

parseSurrender :: Parser (OrderObject Retreat Surrender)
parseSurrender = do
    string "Surrender"
    return $ SurrenderObject

printWithdraw :: OrderObject Retreat Withdraw -> T.Text
printWithdraw (WithdrawObject pt) = T.concat ["- ", printProvinceTarget pt]

parseWithdraw :: Parser (OrderObject Retreat Withdraw)
parseWithdraw = do
    char '-'
    spaces
    pt <- parseProvinceTarget
    return $ WithdrawObject pt

printDisband :: OrderObject Adjust Disband -> T.Text
printDisband DisbandObject = "Disband"

parseDisband :: Parser (OrderObject Adjust Disband)
parseDisband = do
    string "Disband"
    return $ DisbandObject

printBuild :: OrderObject Adjust Build -> T.Text
printBuild BuildObject = "Build"

parseBuild :: Parser (OrderObject Adjust Build)
parseBuild = do
    string "Build"
    return $ BuildObject

printContinue :: OrderObject Adjust Continue -> T.Text
printContinue ContinueObject = "Continue"

parseContinue :: Parser (OrderObject Adjust Continue)
parseContinue = do
    string "Continue"
    return $ ContinueObject
