{-|
Module      : 
Description : 
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)

See http://web.inter.nl.net/users/L.B.Kruijswijk/#6

-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Applicative
import Data.Functor.Identity
import Diplomacy.GreatPower
import Diplomacy.Aligned
import Diplomacy.Province
import Diplomacy.Zone
import Diplomacy.Unit
import Diplomacy.Phase
import Diplomacy.Occupation
import Diplomacy.OrderType
import Diplomacy.Order
import Diplomacy.Subject
import Diplomacy.OrderObject
import Diplomacy.Valid
import Diplomacy.OrderValidation
import Diplomacy.OrderResolution
import Test.HUnit
import Data.Maybe (isJust, isNothing)
import Data.Either (isLeft, isRight)
import Data.List (intersect)
import Data.TypeNat.Nat
import Data.TypeNat.Vect
import Data.AtLeast

tests :: Test
tests = TestList [
      sixA1
    , sixA2
    , sixA3
    , sixA9
    , sixA10
    , sixA11
    , sixA12
    , sixB4
    , sixB5
    , sixB6
    , sixC1
    , sixC2
    , sixC3
    , sixC4
    , sixC5
    , sixC6
    , sixC7
    , sixD1
    , sixD2
    , sixD3
    , sixD4
    , sixD5
    , sixD6
    , sixD7
    , sixD8
    , sixD9
    , sixD10
    , sixD11
    , sixD12
    , sixD13
    , sixD14
    , sixD15
    , sixD16
    , sixD17
    , sixD18
    , sixD19
    , sixD20
    , sixD21
    , sixD22
    , sixD23
    , sixD24
    , sixD25
    , sixD26
    , sixD27
    , sixD28
    , sixD29
    , sixD30
    , sixD33
    , sixD34
    , sixE1
    , sixE2
    , sixE3
    , sixE4
    , sixE5
    , sixE6
    , sixE7
    , sixE8
    , sixE9
    , sixE10
    , sixE11
    , sixE12
    , sixE13
    , sixE14
    , sixE15
    , sixF1
    , sixF2
    , sixF3
    , sixF4
    , sixF5
    , sixF6
    , sixF7
    , sixF8
    , sixF9
    , sixF10
    , sixF11
    , sixF12
    , sixF13
    , sixF14
    , sixF15
    , sixF16
    , sixF17
    , sixF18
    , sixF19
    , sixF20
    , sixF21
    , sixF22
    , sixF23
    , sixF24
    , sixH1
    , sixH2
    , sixH3
    , sixH4
    , sixH5
    , sixH6
    , sixH7
    , sixH8
    , sixH9
    , sixH10
    , sixH11
    , sixH12
    , sixH13
    , sixH14
    , sixH15
    , sixH16
    ]

-- | A helper for testing typical phase resolutions. Give the orders and their
--   expected resolutions once, and get back the actual resolution of those
--   orders.
testTypicalResolution
    :: TypicalResolution 
    -> TypicalResolution
testTypicalResolution expectedRes = actualRes
  where
    actualRes = typicalResolution orders
    orders = M.map mapper expectedRes
    mapper (aunit, SomeResolved (object, _)) = (aunit, SomeOrderObject object)

testRetreatResolution
    :: M.Map Zone (Aligned Unit, SomeResolved OrderObject Retreat)
    -> M.Map Zone (Aligned Unit, SomeResolved OrderObject Retreat)
testRetreatResolution expectedRes = actualRes
  where
    actualRes = retreatResolution orders
    orders = M.map mapper expectedRes
    mapper (aunit, SomeResolved (object, _)) = (aunit, SomeOrderObject object)

-- Moving to an area that is not a neighbour
--
-- England:
--   F North Sea - Picardy
sixA1 :: Test
sixA1 = S.member MoveReachable validation ~? "6.A.1"
  where
    validation = analyze snd (S.singleton . fst) S.empty S.union (moveVOC England occupation) order
    occupation = occupy (Normal NorthSea) (Just $ align Fleet England) emptyOccupation
    order = Order ((Fleet, Normal NorthSea), MoveObject (Normal Picardy))

-- Move army to sea
--
-- England:
--   A Liverpool - Irish Sea
sixA2 :: Test
sixA2 = S.member MoveUnitCanOccupy validation ~? "6.A.2"
  where
    validation = analyze snd (S.singleton . fst) S.empty S.union (moveVOC England occupation) order
    occupation = occupy (Normal Liverpool) (Just $ align Army England) emptyOccupation
    order = Order ((Army, Normal Liverpool), MoveObject (Normal IrishSea))

-- Move fleet to land
--
-- Germany:
--   F Kiel - Munich
sixA3 :: Test
sixA3 = S.member MoveUnitCanOccupy validation ~? "6.A.3"
  where
    validation = analyze snd (S.singleton . fst) S.empty S.union (moveVOC Germany occupation) order
    occupation = occupy (Normal Kiel) (Just $ align Fleet Germany) emptyOccupation
    order = Order ((Fleet, Normal Kiel), MoveObject (Normal Munich))

-- Test cases 6.A.4 and 6.A.5 are moot; this program interprets a loop
-- move as a hold.
-- It's up to front-end programs to make the distinction and block
-- loop moves.
--
-- HOWEVER note that when we add convoy order support, we must ensure
-- that the convoy does not do a loop; you cannot convoy from one place
-- to the same place.

-- Fleets must follow coast if not on sea
--
-- Italy:
--   F Rome - Venice
sixA9 :: Test
sixA9 = S.member MoveReachable validation ~? "6.A.9"
  where
    validation = analyze snd (S.singleton . fst) S.empty S.union (moveVOC Italy occupation) order
    occupation = occupy (Normal Rome) (Just $ align Fleet Italy) emptyOccupation
    order = Order ((Fleet, Normal Rome), MoveObject (Normal Venice))

-- Support on unreachable destination not possible
--
-- Austria:
--   A Venice Hold
--
-- Italy:
--   F Rome Supports A Apulia - Venice
--   A Apulia - Venice
--
-- Support should fail, because Venice cannot be reached from Rome by a
-- fleet.
sixA10 :: Test
sixA10 = S.member SupporterAdjacent validation ~? "6.A.10"
  where
    validation = analyze snd (S.singleton . fst) S.empty S.union (supportVOC Italy occupation) order
    occupation = occupy (Normal Rome) (Just $ align Fleet Italy)
               . occupy (Normal Apulia) (Just $ align Army Italy)
               $ emptyOccupation
    order = Order ((Fleet, Normal Rome), SupportObject (Army, Normal Apulia) (Normal Venice))

-- Simple bounce
--
-- Austria:
--   A Vienna - Tyrolia
--
-- Italy:
--   A Venice - Tyrolia
--
-- The two units bounce.
sixA11 :: Test
sixA11 = (expectedResolution == resolution) ~? "6.A.11"
  where
    resolution = typicalResolution orders
    orders = M.fromList [
          (Zone (Normal Vienna), (align Army Austria, SomeOrderObject (MoveObject (Normal Tyrolia))))
        , (Zone (Normal Venice), (align Army Italy, SomeOrderObject (MoveObject (Normal Tyrolia))))
        ]
    expectedResolution = M.fromList [
          (Zone (Normal Vienna), (align Army Austria, SomeResolved (MoveObject (Normal Tyrolia), Just (MoveBounced (AtLeast (VCons (align (Army, Normal Venice) Italy) VNil) [])))))
        , (Zone (Normal Venice), (align Army Italy, SomeResolved (MoveObject (Normal Tyrolia), Just (MoveBounced (AtLeast (VCons (align (Army, Normal Vienna) Austria) VNil) [])))))

        ]

-- Bounce of three units
--
-- Austria:
--   A Vienna - Tyrolia
--
-- Germany:
--   A Munich - Tyrolia
--
-- Italy:
--   A Venice - Tyrolia
--
-- The two units bounce.
sixA12 :: Test
sixA12 = (expectedResolution == resolution) ~? "6.A.12"
  where
    resolution = typicalResolution orders
    orders = M.fromList [
          (Zone (Normal Vienna), (align Army Austria, SomeOrderObject (MoveObject (Normal Tyrolia))))
        , (Zone (Normal Venice), (align Army Italy, SomeOrderObject (MoveObject (Normal Tyrolia))))
        , (Zone (Normal Munich), (align Army Germany, SomeOrderObject (MoveObject (Normal Tyrolia))))
        ]
    expectedResolution = M.fromList [
          (Zone (Normal Vienna), (align Army Austria, SomeResolved (MoveObject (Normal Tyrolia), Just (MoveBounced (AtLeast (VCons (align (Army, Normal Venice) Italy) VNil) [align (Army, Normal Munich) Germany])))))
        , (Zone (Normal Venice), (align Army Italy, SomeResolved (MoveObject (Normal Tyrolia), Just (MoveBounced (AtLeast (VCons (align (Army, Normal Vienna) Austria) VNil) [align (Army, Normal Munich) Germany])))))
        , (Zone (Normal Munich), (align Army Germany, SomeResolved (MoveObject (Normal Tyrolia), Just (MoveBounced (AtLeast (VCons (align (Army, Normal Vienna) Austria) VNil) [align (Army, Normal Venice) Italy])))))

        ]

-- 6.B.1 is ruled out by validation.
--
--   F Portual - Spain
--
-- is invalid because a fleet cannot occupy Spain; it can occupy north or
-- south coast of Spain.

-- 6.B.2 is ignored.
--
--   F Gascony - Spain
--
-- is invalid because a fleet cannot occupy Spain; same reason as in 6.B.1.
-- The test case indicates a preference for defaulting to the only possible
-- coast, but that's not the responsibility of our solver; a frontend can
-- do the defaulting if it wishes.

-- 6.B.3 is ruled out by validation.
--
--   F Gascony - Spain (sc)
--
-- is invalid because a fleet cannot possibly reach the south coast of Spain
-- from Gascony.

-- 6.B.4 Support to unreachable coast allowed
--
-- France:
--   F Gascony - Spain (North Coast)
--   F Marseilles Supports F Gascony - Spain (North Coast)
--
-- Italy:
--   F Western Mediterranean - Spain (North Coast)
--
-- Support passes; France's move succeeds.
-- This tests validation AND resolution. Not only must the support be valid, it
-- must also cause the French move to succeed and the Italian move to fail.
sixB4 :: Test
sixB4 = (S.null supportValidation && resolution == expectedResolution) ~? "6.B.4"
  where

    supportValidation = analyze snd (S.singleton . fst) S.empty S.union (supportVOC France occupation) supportOrder

    supportOrder = Order ((Fleet, Normal Marseilles), SupportObject (Fleet, Normal Gascony) (Special SpainNorth))

    occupation = occupy (Normal Gascony) (Just $ align Fleet France)
               . occupy (Normal Marseilles) (Just $ align Fleet France)
               . occupy (Normal WesternMediterranean) (Just $ align Fleet Italy)
               $ emptyOccupation

    resolution = typicalResolution orders

    orders = M.fromList [
          (Zone (Normal Gascony), (align Fleet France, SomeOrderObject (MoveObject (Special SpainNorth))))
        , (Zone (Normal Marseilles), (align Fleet France, SomeOrderObject (SupportObject (Fleet, Normal Gascony) (Special SpainNorth))))
        , (Zone (Normal WesternMediterranean), (align Fleet Italy, SomeOrderObject (MoveObject (Special SpainNorth))))
        ]

    expectedResolution :: TypicalResolution
    expectedResolution = M.fromList [
          (Zone (Normal Gascony), (align Fleet France, SomeResolved (MoveObject (Special SpainNorth), Nothing)))
        , (Zone (Normal Marseilles), (align Fleet France, SomeResolved (SupportObject (Fleet, Normal Gascony) (Special SpainNorth), Nothing)))
        , (Zone (Normal WesternMediterranean), (align Fleet Italy, SomeResolved (MoveObject (Special SpainNorth), Just (MoveOverpowered (AtLeast (VCons (align (Fleet, Normal Gascony) France) VNil) [])))))
        ]

-- 6.B.5 Support from unreachable coast not allowed
--
-- France:
--   F Marseilles - Gulf of Lyon
--   F Spain (North Coast) Supports F Marseilles - Gulf of Lyon
--
-- Italy:
--   F Gulf of Lyon Hold
--
-- Support should fail because Gulf of Lyon cannot be reached from
-- Spain's north coast.
-- This case involves only validation; trying to resolve these orders is (in
-- the context of these test cases) undefined behaviour, since resolution
-- respects the diplomacy rules only if the orders are valid.
sixB5 :: Test
sixB5 = S.member SupporterAdjacent validation ~? "6.B.5"
  where

    validation = analyze snd (S.singleton . fst) S.empty S.union (supportVOC France occupation) support
 
    support = Order ((Fleet, Special SpainNorth), SupportObject (Fleet, Normal Marseilles) (Normal GulfOfLyon))

    occupation = occupy (Normal Marseilles) (Just $ align Fleet France)
               . occupy (Special SpainNorth) (Just $ align Fleet France)
               $ emptyOccupation

-- 6.B.6. SUPPORT CAN BE CUT WITH OTHER COAST
--
-- Support can be cut from the other coast.
--
-- England: 
--   F Irish Sea Supports F North Atlantic Ocean - Mid-Atlantic Ocean
--   F North Atlantic Ocean - Mid-Atlantic Ocean
--
-- France: 
--   F Spain(nc) Supports F Mid-Atlantic Ocean
--   F Mid-Atlantic Ocean Hold
--
-- Italy: 
--   F Gulf of Lyon - Spain(sc)
--
-- The Italian fleet in the Gulf of Lyon will cut the support in Spain.
-- That means that the French fleet in the Mid Atlantic Ocean will be dislodged
-- by the English fleet in the North Atlantic Ocean. 
sixB6 :: Test
sixB6 = (expectedResolution == resolution) ~? "6.B.6"
  where

    resolution = typicalResolution orders

    orders = M.fromList [
          (Zone (Normal IrishSea), (align Fleet England, SomeOrderObject (SupportObject (Fleet, Normal NorthAtlanticOcean) (Normal MidAtlanticOcean))))
        , (Zone (Normal NorthAtlanticOcean), (align Fleet England, SomeOrderObject (MoveObject (Normal MidAtlanticOcean))))

        , (Zone (Special SpainNorth), (align Fleet France, SomeOrderObject (SupportObject (Fleet, Normal MidAtlanticOcean) (Normal MidAtlanticOcean))))
        , (Zone (Normal MidAtlanticOcean), (align Fleet France, SomeOrderObject (MoveObject (Normal MidAtlanticOcean))))

        , (Zone (Normal GulfOfLyon), (align Fleet Italy, SomeOrderObject (MoveObject (Special SpainSouth))))
        ]

    expectedResolution = M.fromList [
          (Zone (Normal IrishSea), (align Fleet England, SomeResolved (SupportObject (Fleet, Normal NorthAtlanticOcean) (Normal MidAtlanticOcean), Nothing)))
        , (Zone (Normal NorthAtlanticOcean), (align Fleet England, SomeResolved (MoveObject (Normal MidAtlanticOcean), Nothing)))

        , (Zone (Special SpainNorth), (align Fleet France, SomeResolved (SupportObject (Fleet, Normal MidAtlanticOcean) (Normal MidAtlanticOcean), Just (SupportCut (AtLeast (VCons (align (Fleet, Normal GulfOfLyon) Italy) VNil) [])))))
        , (Zone (Normal MidAtlanticOcean), (align Fleet France, SomeResolved (MoveObject (Normal MidAtlanticOcean), Just (MoveOverpowered (AtLeast (VCons (align (Fleet, Normal NorthAtlanticOcean) England) VNil) [])))))

        , (Zone (Normal GulfOfLyon), (align Fleet Italy, SomeResolved (MoveObject (Special SpainSouth), Just (MoveBounced (AtLeast (VCons (align (Fleet, Special SpainNorth) France) VNil) [])))))
        ]

-- Cannot change coasts; this must fail.
changeCoasts :: Test
changeCoasts = S.member MoveReachable validation ~? "changeCoasts"
  where
    validation = analyze snd (S.singleton . fst) S.empty S.union (moveVOC France occupation) order
    occupation = occupy (Special SpainNorth) (Just (align Fleet France)) emptyOccupation
    order = Order ((Fleet, Special SpainNorth), MoveObject (Special SpainSouth))

-- 6.C.1. TEST CASE, THREE ARMY CIRCULAR MOVEMENT
--
-- Three units can change place, even in spring 1901.
--
-- Turkey: 
-- F Ankara - Constantinople
-- A Constantinople - Smyrna
-- A Smyrna - Ankara
--
-- All three units will move.
sixC1 :: Test
sixC1 = (expectedResolution == resolution) ~? "6.C.1"
  where

    resolution = typicalResolution orders

    orders = M.fromList [
          (Zone (Normal Ankara), (align Fleet Turkey, SomeOrderObject (MoveObject (Normal Constantinople))))
        , (Zone (Normal Constantinople), (align Army Turkey, SomeOrderObject (MoveObject (Normal Smyrna))))
        , (Zone (Normal Smyrna), (align Army Turkey, SomeOrderObject (MoveObject (Normal Ankara))))
        ]

    expectedResolution = M.fromList [
          (Zone (Normal Ankara), (align Fleet Turkey, SomeResolved (MoveObject (Normal Constantinople), Nothing)))
        , (Zone (Normal Constantinople), (align Army Turkey, SomeResolved (MoveObject (Normal Smyrna), Nothing)))
        , (Zone (Normal Smyrna), (align Army Turkey, SomeResolved (MoveObject (Normal Ankara), Nothing)))
        ]


-- 6.C.2. TEST CASE, THREE ARMY CIRCULAR MOVEMENT WITH SUPPORT
--
-- Three units can change place, even when one gets support.
--
-- Turkey: 
-- F Ankara - Constantinople
-- A Constantinople - Smyrna
-- A Smyrna - Ankara
-- A Bulgaria Supports F Ankara - Constantinople
--
-- Of course the three units will move, but knowing how programs are written, this can confuse the adjudicator. 
sixC2 :: Test
sixC2 = (expectedResolution == resolution) ~? "6.C.2"
  where

    resolution = typicalResolution orders

    orders = M.fromList [
          (Zone (Normal Ankara), (align Fleet Turkey, SomeOrderObject (MoveObject (Normal Constantinople))))
        , (Zone (Normal Constantinople), (align Army Turkey, SomeOrderObject (MoveObject (Normal Smyrna))))
        , (Zone (Normal Smyrna), (align Army Turkey, SomeOrderObject (MoveObject (Normal Ankara))))
        , (Zone (Normal Bulgaria), (align Army Turkey, SomeOrderObject (SupportObject (Fleet, Normal Ankara) (Normal Constantinople))))
        ]

    expectedResolution = M.fromList [
          (Zone (Normal Ankara), (align Fleet Turkey, SomeResolved (MoveObject (Normal Constantinople), Nothing)))
        , (Zone (Normal Constantinople), (align Army Turkey, SomeResolved (MoveObject (Normal Smyrna), Nothing)))
        , (Zone (Normal Smyrna), (align Army Turkey, SomeResolved (MoveObject (Normal Ankara), Nothing)))
        , (Zone (Normal Bulgaria), (align Army Turkey, SomeResolved (SupportObject (Fleet, Normal Ankara) (Normal Constantinople), Nothing)))
        ]

-- 6.C.3. TEST CASE, A DISRUPTED THREE ARMY CIRCULAR MOVEMENT
--
-- When one of the units bounces, the whole circular movement will hold.
--
-- Turkey: 
--   F Ankara - Constantinople
--   A Constantinople - Smyrna
--   A Smyrna - Ankara
--   A Bulgaria - Constantinople
--
-- All moves fail.
sixC3 :: Test
sixC3 = (expectedResolution == resolution) ~? "6.C.3"
  where

    resolution = typicalResolution orders

    orders = M.fromList [
          (Zone (Normal Ankara), (align Fleet Turkey, SomeOrderObject (MoveObject (Normal Constantinople))))
        , (Zone (Normal Constantinople), (align Army Turkey, SomeOrderObject (MoveObject (Normal Smyrna))))
        , (Zone (Normal Smyrna), (align Army Turkey, SomeOrderObject (MoveObject (Normal Ankara))))
        , (Zone (Normal Bulgaria), (align Army Turkey, SomeOrderObject (MoveObject (Normal Constantinople))))
        ]

    expectedResolution = M.fromList [
          (Zone (Normal Ankara), (align Fleet Turkey, SomeResolved (MoveObject (Normal Constantinople), Just (MoveBounced (AtLeast (VCons (align (Army, Normal Bulgaria) Turkey) VNil) [])))))
        , (Zone (Normal Constantinople), (align Army Turkey, SomeResolved (MoveObject (Normal Smyrna), Just (MoveBounced (AtLeast (VCons (align (Army, Normal Smyrna) Turkey) VNil) [])))))
        , (Zone (Normal Smyrna), (align Army Turkey, SomeResolved (MoveObject (Normal Ankara), Just (MoveBounced (AtLeast (VCons (align (Fleet, Normal Ankara) Turkey) VNil) [])))))
        , (Zone (Normal Bulgaria), (align Army Turkey, SomeResolved (MoveObject (Normal Constantinople), Just (MoveBounced (AtLeast (VCons (align (Fleet, Normal Ankara) Turkey) VNil) [])))))
        ]

-- 6.C.4. TEST CASE, A CIRCULAR MOVEMENT WITH ATTACKED CONVOY
--
-- When the circular movement contains an attacked convoy, the circular movement succeeds.
--
-- Austria: 
-- A Trieste - Serbia
-- A Serbia - Bulgaria
--
-- Turkey: 
-- A Bulgaria - Trieste
-- F Aegean Sea Convoys A Bulgaria - Trieste
-- F Ionian Sea Convoys A Bulgaria - Trieste
-- F Adriatic Sea Convoys A Bulgaria - Trieste
--
-- Italy: 
-- F Naples - Ionian Sea
--
-- The fleet in the Ionian Sea is attacked but not dislodged. The circular movement succeeds. The Austrian and Turkish armies will advance.
sixC4 :: Test
sixC4 = (expectedResolution == resolution) ~? "6.C.4"
  where

    resolution = typicalResolution orders

    orders = M.fromList [
          (Zone (Normal Trieste), (align Army Austria, SomeOrderObject (MoveObject (Normal Serbia))))
        , (Zone (Normal Serbia), (align Army Austria, SomeOrderObject (MoveObject (Normal Bulgaria))))

        , (Zone (Normal Bulgaria), (align Army Turkey, SomeOrderObject (MoveObject (Normal Trieste))))
        , (Zone (Normal AegeanSea), (align Fleet Turkey, SomeOrderObject (ConvoyObject (Army, Normal Bulgaria) (Normal Trieste))))
        , (Zone (Normal IonianSea), (align Fleet Turkey, SomeOrderObject (ConvoyObject (Army, Normal Bulgaria) (Normal Trieste))))
        , (Zone (Normal AdriaticSea), (align Fleet Turkey, SomeOrderObject (ConvoyObject (Army, Normal Bulgaria) (Normal Trieste))))

        , (Zone (Normal Naples), (align Fleet Italy, SomeOrderObject (MoveObject (Normal IonianSea))))
        ]

    expectedResolution = M.fromList [
          (Zone (Normal Trieste), (align Army Austria, SomeResolved (MoveObject (Normal Serbia), Nothing)))
        , (Zone (Normal Serbia), (align Army Austria, SomeResolved (MoveObject (Normal Bulgaria), Nothing)))

        , (Zone (Normal Bulgaria), (align Army Turkey, SomeResolved (MoveObject (Normal Trieste), Nothing)))
        , (Zone (Normal AegeanSea), (align Fleet Turkey, SomeResolved (ConvoyObject (Army, Normal Bulgaria) (Normal Trieste), Nothing)))
        , (Zone (Normal IonianSea), (align Fleet Turkey, SomeResolved (ConvoyObject (Army, Normal Bulgaria) (Normal Trieste), Nothing)))
        , (Zone (Normal AdriaticSea), (align Fleet Turkey, SomeResolved (ConvoyObject (Army, Normal Bulgaria) (Normal Trieste), Nothing)))

        , (Zone (Normal Naples), (align Fleet Italy, SomeResolved (MoveObject (Normal IonianSea), Just (MoveBounced (AtLeast (VCons (align (Fleet, Normal IonianSea) Turkey) VNil) [])))))
        ]

-- 6.C.5. TEST CASE, A DISRUPTED CIRCULAR MOVEMENT DUE TO DISLODGED CONVOY
--
-- When the circular movement contains a convoy, the circular movement is disrupted when the convoying fleet is dislodged.
--
-- Austria: 
-- A Trieste - Serbia
-- A Serbia - Bulgaria
--
-- Turkey: 
-- A Bulgaria - Trieste
-- F Aegean Sea Convoys A Bulgaria - Trieste
-- F Ionian Sea Convoys A Bulgaria - Trieste
-- F Adriatic Sea Convoys A Bulgaria - Trieste
--
-- Italy: 
-- F Naples - Ionian Sea
-- F Tunis Supports F Naples - Ionian Sea
--
-- Due to the dislodged convoying fleet, all Austrian and Turkish armies will not move. 
sixC5 :: Test
sixC5 = (expectedResolution == resolution) ~? "6.C.5"
  where

    resolution = typicalResolution orders

    orders = M.fromList [
          (Zone (Normal Trieste), (align Army Austria, SomeOrderObject (MoveObject (Normal Serbia))))
        , (Zone (Normal Serbia), (align Army Austria, SomeOrderObject (MoveObject (Normal Bulgaria))))

        , (Zone (Normal Bulgaria), (align Army Turkey, SomeOrderObject (MoveObject (Normal Trieste))))
        , (Zone (Normal AegeanSea), (align Fleet Turkey, SomeOrderObject (ConvoyObject (Army, Normal Bulgaria) (Normal Trieste))))
        , (Zone (Normal IonianSea), (align Fleet Turkey, SomeOrderObject (ConvoyObject (Army, Normal Bulgaria) (Normal Trieste))))
        , (Zone (Normal AdriaticSea), (align Fleet Turkey, SomeOrderObject (ConvoyObject (Army, Normal Bulgaria) (Normal Trieste))))

        , (Zone (Normal Naples), (align Fleet Italy, SomeOrderObject (MoveObject (Normal IonianSea))))
        , (Zone (Normal Tunis), (align Fleet Italy, SomeOrderObject (SupportObject (Fleet, Normal Naples) (Normal IonianSea))))
        ]

    expectedResolution = M.fromList [
          (Zone (Normal Trieste), (align Army Austria, SomeResolved (MoveObject (Normal Serbia), Just (MoveBounced (AtLeast (VCons (align (Army, Normal Serbia) Austria) VNil) [])))))
        , (Zone (Normal Serbia), (align Army Austria, SomeResolved (MoveObject (Normal Bulgaria), Just (MoveBounced (AtLeast (VCons (align (Army, Normal Bulgaria) Turkey) VNil) [])))))

        , (Zone (Normal Bulgaria), (align Army Turkey, SomeResolved (MoveObject (Normal Trieste), Just (MoveNoConvoy))))
        , (Zone (Normal AegeanSea), (align Fleet Turkey, SomeResolved (ConvoyObject (Army, Normal Bulgaria) (Normal Trieste), Just (ConvoyRouteCut [(Zone (Normal IonianSea), align (Fleet, Normal Naples) Italy)]))))
        , (Zone (Normal IonianSea), (align Fleet Turkey, SomeResolved (ConvoyObject (Army, Normal Bulgaria) (Normal Trieste), Just (ConvoyRouteCut [(Zone (Normal IonianSea), align (Fleet, Normal Naples) Italy)]))))
        , (Zone (Normal AdriaticSea), (align Fleet Turkey, SomeResolved (ConvoyObject (Army, Normal Bulgaria) (Normal Trieste), Just (ConvoyRouteCut [(Zone (Normal IonianSea), align (Fleet, Normal Naples) Italy)]))))

        , (Zone (Normal Naples), (align Fleet Italy, SomeResolved (MoveObject (Normal IonianSea), Nothing)))
        , (Zone (Normal Tunis), (align Fleet Italy, SomeResolved (SupportObject (Fleet, Normal Naples) (Normal IonianSea), Nothing)))
        ]

-- 6.C.6. TEST CASE, TWO ARMIES WITH TWO CONVOYS
--
-- Two armies can swap places even when they are not adjacent.
--
-- England: 
-- F North Sea Convoys A London - Belgium
-- A London - Belgium
--
-- France: 
-- F English Channel Convoys A Belgium - London
-- A Belgium - London
--
-- Both convoys should succeed. 
sixC6 = (expectedResolution == resolution) ~? "6.C.6"
  where

    resolution = typicalResolution orders

    orders = M.fromList [
          (Zone (Normal NorthSea), (align Fleet England, SomeOrderObject (ConvoyObject (Army, Normal London) (Normal Belgium))))
        , (Zone (Normal London), (align Army England, SomeOrderObject (MoveObject (Normal Belgium))))

        , (Zone (Normal EnglishChannel), (align Fleet France, SomeOrderObject (ConvoyObject (Army, Normal Belgium) (Normal London))))
        , (Zone (Normal Belgium), (align Army France, SomeOrderObject (MoveObject (Normal London))))
        ]

    expectedResolution = M.fromList [
          (Zone (Normal NorthSea), (align Fleet England, SomeResolved (ConvoyObject (Army, Normal London) (Normal Belgium), Nothing)))
        , (Zone (Normal London), (align Army England, SomeResolved (MoveObject (Normal Belgium), Nothing)))

        , (Zone (Normal EnglishChannel), (align Fleet France, SomeResolved (ConvoyObject (Army, Normal Belgium) (Normal London), Nothing)))
        , (Zone (Normal Belgium), (align Army France, SomeResolved (MoveObject (Normal London), Nothing)))
        ]

-- 6.C.7. TEST CASE, DISRUPTED UNIT SWAP
--
-- If in a swap one of the unit bounces, then the swap fails.
--
-- England: 
-- F North Sea Convoys A London - Belgium
-- A London - Belgium
--
-- France: 
-- F English Channel Convoys A Belgium - London
-- A Belgium - London
-- A Burgundy - Belgium
--
-- None of the units will succeed to move. 
sixC7 :: Test
sixC7 = (expectedResolution == resolution) ~? "6.C.7"
  where

    resolution = typicalResolution orders

    orders = M.fromList [
          (Zone (Normal NorthSea), (align Fleet England, SomeOrderObject (ConvoyObject (Army, Normal London) (Normal Belgium))))
        , (Zone (Normal London), (align Army England, SomeOrderObject (MoveObject (Normal Belgium))))

        , (Zone (Normal EnglishChannel), (align Fleet France, SomeOrderObject (ConvoyObject (Army, Normal Belgium) (Normal London))))
        , (Zone (Normal Belgium), (align Army France, SomeOrderObject (MoveObject (Normal London))))
        , (Zone (Normal Burgundy), (align Army France, SomeOrderObject (MoveObject (Normal Belgium))))
        ]

    expectedResolution = M.fromList [
        -- Note that a Convoy can succeed even if the move which it would convoy
        -- does not succeed.
          (Zone (Normal NorthSea), (align Fleet England, SomeResolved (ConvoyObject (Army, Normal London) (Normal Belgium), Nothing)))
        , (Zone (Normal London), (align Army England, SomeResolved (MoveObject (Normal Belgium), Just (MoveBounced (AtLeast (VCons (align (Army, Normal Burgundy) France) VNil) [])))))

        , (Zone (Normal EnglishChannel), (align Fleet France, SomeResolved (ConvoyObject (Army, Normal Belgium) (Normal London), Nothing)))
        , (Zone (Normal Belgium), (align Army France, SomeResolved (MoveObject (Normal London), Just (MoveBounced (AtLeast (VCons (align (Army, Normal London) England) VNil) [])))))
        , (Zone (Normal Burgundy), (align Army France, SomeResolved (MoveObject (Normal Belgium), Just (MoveBounced (AtLeast (VCons (align (Army, Normal London) England) VNil) [])))))
        ]

-- 6.D.1. TEST CASE, SUPPORTED HOLD CAN PREVENT DISLODGEMENT
--
-- The most simple support to hold order.
--
-- Austria: 
-- F Adriatic Sea Supports A Trieste - Venice
-- A Trieste - Venice
--
-- Italy: 
-- A Venice Hold
-- A Tyrolia Supports A Venice
--
-- The support of Tyrolia prevents that the army in Venice is dislodged. The army in Trieste will not move. 
sixD1 :: Test
sixD1 = (expectedResolution == resolution) ~? "6.D.1"
  where

    resolution = typicalResolution orders

    orders = M.fromList [
          (Zone (Normal AdriaticSea), (align Fleet Austria, SomeOrderObject (SupportObject (Army, Normal Trieste) (Normal Venice))))
        , (Zone (Normal Trieste), (align Army Austria, SomeOrderObject (MoveObject (Normal Venice))))

        , (Zone (Normal Venice), (align Army Italy, SomeOrderObject (MoveObject (Normal Venice))))
        , (Zone (Normal Tyrolia), (align Army Italy, SomeOrderObject (SupportObject (Army, Normal Venice) (Normal Venice))))
        ]

    expectedResolution = M.fromList [
          (Zone (Normal AdriaticSea), (align Fleet Austria, SomeResolved (SupportObject (Army, Normal Trieste) (Normal Venice), Nothing)))
        , (Zone (Normal Trieste), (align Army Austria, SomeResolved (MoveObject (Normal Venice), Just (MoveBounced (AtLeast (VCons (align (Army, Normal Venice) Italy) VNil) [])))))

        , (Zone (Normal Venice), (align Army Italy, SomeResolved (MoveObject (Normal Venice), Nothing)))
        , (Zone (Normal Tyrolia), (align Army Italy, SomeResolved (SupportObject (Army, Normal Venice) (Normal Venice), Nothing)))
        ]

-- 6.D.2. TEST CASE, A MOVE CUTS SUPPORT ON HOLD
--
-- The most simple support on hold cut.
--
-- Austria: 
-- F Adriatic Sea Supports A Trieste - Venice
-- A Trieste - Venice
-- A Vienna - Tyrolia
--
-- Italy: 
-- A Venice Hold
-- A Tyrolia Supports A Venice
--
-- The support of Tyrolia is cut by the army in Vienna. That means that the army in Venice is dislodged by the army from Trieste. 
sixD2 = (expectedResolution == resolution) ~? "6.D.2"
  where

    resolution = typicalResolution orders

    orders = M.fromList [
          (Zone (Normal AdriaticSea), (align Fleet Austria, SomeOrderObject (SupportObject (Army, Normal Trieste) (Normal Venice))))
        , (Zone (Normal Trieste), (align Army Austria, SomeOrderObject (MoveObject (Normal Venice))))
        , (Zone (Normal Vienna), (align Army Austria, SomeOrderObject (MoveObject (Normal Tyrolia))))

        , (Zone (Normal Venice), (align Army Italy, SomeOrderObject (MoveObject (Normal Venice))))
        , (Zone (Normal Tyrolia), (align Army Italy, SomeOrderObject (SupportObject (Army, Normal Venice) (Normal Venice))))
        ]

    expectedResolution = M.fromList [
          (Zone (Normal AdriaticSea), (align Fleet Austria, SomeResolved (SupportObject (Army, Normal Trieste) (Normal Venice), Nothing)))
        , (Zone (Normal Trieste), (align Army Austria, SomeResolved (MoveObject (Normal Venice), Nothing)))
        , (Zone (Normal Vienna), (align Army Austria, SomeResolved (MoveObject (Normal Tyrolia), Just (MoveBounced (AtLeast (VCons (align (Army, Normal Tyrolia) Italy) VNil) [])))))

        , (Zone (Normal Venice), (align Army Italy, SomeResolved (MoveObject (Normal Venice), Just (MoveOverpowered (AtLeast (VCons (align (Army, Normal Trieste) Austria) VNil) [])))))
        , (Zone (Normal Tyrolia), (align Army Italy, SomeResolved (SupportObject (Army, Normal Venice) (Normal Venice), Just (SupportCut (AtLeast (VCons (align (Army, Normal Vienna) Austria) VNil) [])))))
        ]

-- 6.D.3. TEST CASE, A MOVE CUTS SUPPORT ON MOVE
--
-- The most simple support on move cut.
--
-- Austria: 
-- F Adriatic Sea Supports A Trieste - Venice
-- A Trieste - Venice
--
-- Italy: 
-- A Venice Hold
-- F Ionian Sea - Adriatic Sea
--
-- The support of the fleet in the Adriatic Sea is cut. That means that the army in Venice will not be dislodged and the army in Trieste stays in Trieste. 
sixD3 = (expectedResolution == resolution) ~? "6.D.3"
  where

    resolution = typicalResolution orders

    orders = M.fromList [
          (Zone (Normal AdriaticSea), (align Fleet Austria, SomeOrderObject (SupportObject (Army, Normal Trieste) (Normal Venice))))
        , (Zone (Normal Trieste), (align Army Austria, SomeOrderObject (MoveObject (Normal Venice))))

        , (Zone (Normal Venice), (align Army Italy, SomeOrderObject (MoveObject (Normal Venice))))
        , (Zone (Normal IonianSea), (align Fleet Italy, SomeOrderObject (MoveObject (Normal AdriaticSea))))
        ]

    expectedResolution = M.fromList [
          (Zone (Normal AdriaticSea), (align Fleet Austria, SomeResolved (SupportObject (Army, Normal Trieste) (Normal Venice), Just (SupportCut (AtLeast (VCons (align (Fleet, Normal IonianSea) Italy) VNil) [])))))
        , (Zone (Normal Trieste), (align Army Austria, SomeResolved (MoveObject (Normal Venice), Just (MoveBounced (AtLeast (VCons (align (Army, Normal Venice) Italy) VNil) [])))))

        , (Zone (Normal Venice), (align Army Italy, SomeResolved (MoveObject (Normal Venice), Nothing)))
        , (Zone (Normal IonianSea), (align Fleet Italy, SomeResolved (MoveObject (Normal AdriaticSea), Just (MoveBounced (AtLeast (VCons (align (Fleet, Normal AdriaticSea) Austria) VNil) [])))))
        ]

-- 6.D.4. TEST CASE, SUPPORT TO HOLD ON UNIT SUPPORTING A HOLD ALLOWED
--
-- A unit that is supporting a hold, can receive a hold support.
--
-- Germany: 
-- A Berlin Supports F Kiel
-- F Kiel Supports A Berlin
--
-- Russia: 
-- F Baltic Sea Supports A Prussia - Berlin
-- A Prussia - Berlin
--
-- The Russian move from Prussia to Berlin fails. 
sixD4 = (expectedResolution == resolution) ~? "6.D.4"
  where

    resolution = typicalResolution orders

    orders = M.fromList [
          (Zone (Normal Berlin), (align Army Germany, SomeOrderObject (SupportObject (Fleet, Normal Kiel) (Normal Kiel))))
        , (Zone (Normal Kiel), (align Fleet Germany, SomeOrderObject (SupportObject (Army, Normal Berlin) (Normal Berlin))))

        , (Zone (Normal BalticSea), (align Fleet Russia, SomeOrderObject (SupportObject (Army, Normal Prussia) (Normal Berlin))))
        , (Zone (Normal Prussia), (align Army Russia, SomeOrderObject (MoveObject (Normal Berlin))))
        ]

    expectedResolution = M.fromList [
          (Zone (Normal Berlin), (align Army Germany, SomeResolved (SupportObject (Fleet, Normal Kiel) (Normal Kiel), Just (SupportCut (AtLeast (VCons (align (Army, Normal Prussia) Russia) VNil) [])))))
        , (Zone (Normal Kiel), (align Fleet Germany, SomeResolved (SupportObject (Army, Normal Berlin) (Normal Berlin), Nothing)))

        , (Zone (Normal BalticSea), (align Fleet Russia, SomeResolved (SupportObject (Army, Normal Prussia) (Normal Berlin), Nothing)))
        , (Zone (Normal Prussia), (align Army Russia, SomeResolved (MoveObject (Normal Berlin), Just (MoveBounced (AtLeast (VCons (align (Army, Normal Berlin) Germany) VNil) [])))))
        ]

-- 6.D.5. TEST CASE, SUPPORT TO HOLD ON UNIT SUPPORTING A MOVE ALLOWED
--
-- A unit that is supporting a move, can receive a hold support.
--
-- Germany: 
-- A Berlin Supports A Munich - Silesia
-- F Kiel Supports A Berlin
-- A Munich - Silesia
--
-- Russia: 
-- F Baltic Sea Supports A Prussia - Berlin
-- A Prussia - Berlin
--
-- The Russian move from Prussia to Berlin fails. (German move to Silesia
-- succeeds).
sixD5 = (expectedResolution == resolution) ~? "6.D.5"
  where

    resolution = typicalResolution orders

    orders = M.fromList [
          (Zone (Normal Berlin), (align Army Germany, SomeOrderObject (SupportObject (Army, Normal Munich) (Normal Silesia))))
        , (Zone (Normal Kiel), (align Fleet Germany, SomeOrderObject (SupportObject (Army, Normal Berlin) (Normal Berlin))))
        , (Zone (Normal Munich), (align Army Germany, SomeOrderObject (MoveObject (Normal Silesia))))

        , (Zone (Normal BalticSea), (align Fleet Russia, SomeOrderObject (SupportObject (Army, Normal Prussia) (Normal Berlin))))
        , (Zone (Normal Prussia), (align Army Russia, SomeOrderObject (MoveObject (Normal Berlin))))
        ]

    expectedResolution = M.fromList [
          (Zone (Normal Berlin), (align Army Germany, SomeResolved (SupportObject (Army, Normal Munich) (Normal Silesia), Just (SupportCut (AtLeast (VCons (align (Army, Normal Prussia) Russia) VNil) [])))))
        , (Zone (Normal Kiel), (align Fleet Germany, SomeResolved (SupportObject (Army, Normal Berlin) (Normal Berlin), Nothing)))
        , (Zone (Normal Munich), (align Army Germany, SomeResolved (MoveObject (Normal Silesia), Nothing)))

        , (Zone (Normal BalticSea), (align Fleet Russia, SomeResolved (SupportObject (Army, Normal Prussia) (Normal Berlin), Nothing)))
        , (Zone (Normal Prussia), (align Army Russia, SomeResolved (MoveObject (Normal Berlin), Just (MoveBounced (AtLeast (VCons (align (Army, Normal Berlin) Germany) VNil) [])))))
        ]

-- 6.D.6. TEST CASE, SUPPORT TO HOLD ON CONVOYING UNIT ALLOWED
--
-- A unit that is convoying, can receive a hold support.
--
-- Germany: 
-- A Berlin - Sweden
-- F Baltic Sea Convoys A Berlin - Sweden
-- F Prussia Supports F Baltic Sea
--
-- Russia: 
-- F Livonia - Baltic Sea
-- F Gulf of Bothnia Supports F Livonia - Baltic Sea
--
-- The Russian move from Livonia to the Baltic Sea fails. The convoy from Berlin to Sweden succeeds. 
sixD6 = (expectedResolution == testTypicalResolution expectedResolution) ~? "6.D.6"
  where

    expectedResolution = M.fromList [
          (Zone (Normal Berlin), (align Army Germany, SomeResolved (MoveObject (Normal Sweden), Nothing)))
        , (Zone (Normal BalticSea), (align Fleet Germany, SomeResolved (ConvoyObject (Army, Normal Berlin) (Normal Sweden), Nothing)))
        , (Zone (Normal Prussia), (align Fleet Germany, SomeResolved (SupportObject (Fleet, Normal BalticSea) (Normal BalticSea), Nothing)))

        , (Zone (Normal Livonia), (align Fleet Russia, SomeResolved (MoveObject (Normal BalticSea), Just (MoveBounced (AtLeast (VCons (align (Fleet, Normal BalticSea) Germany) VNil) [])))))
        , (Zone (Normal GulfOfBothnia), (align Fleet Russia, SomeResolved (SupportObject (Fleet, Normal Livonia) (Normal BalticSea), Nothing)))
        ]

-- 6.D.7. TEST CASE, SUPPORT TO HOLD ON MOVING UNIT NOT ALLOWED
--
-- A unit that is moving, can not receive a hold support for the situation that the move fails.
--
-- Germany: 
-- F Baltic Sea - Sweden
-- F Prussia Supports F Baltic Sea
--
-- Russia: 
-- F Livonia - Baltic Sea
-- F Gulf of Bothnia Supports F Livonia - Baltic Sea
-- A Finland - Sweden
--
-- The support of the fleet in Prussia fails. The fleet in Baltic Sea will bounce on the Russian army in Finland and will be dislodged by the Russian fleet from Livonia when it returns to the Baltic Sea. 
sixD7 = (expectedResolution == testTypicalResolution expectedResolution) ~? "6.D.7"
  where
    
    expectedResolution = M.fromList [
          (Zone (Normal BalticSea), (align Fleet Germany, SomeResolved (MoveObject (Normal Sweden), Just (MoveBounced (AtLeast (VCons (align (Army, Normal Finland) Russia) VNil) [])))))
        , (Zone (Normal Prussia), (align Fleet Germany, SomeResolved (SupportObject (Fleet, Normal BalticSea) (Normal BalticSea), Just SupportVoid)))

        , (Zone (Normal Livonia), (align Fleet Russia, SomeResolved (MoveObject (Normal BalticSea), Nothing)))
        , (Zone (Normal GulfOfBothnia), (align Fleet Russia, SomeResolved (SupportObject (Fleet, Normal Livonia) (Normal BalticSea), Nothing)))
        , (Zone (Normal Finland), (align Army Russia, SomeResolved (MoveObject (Normal Sweden), Just (MoveBounced (AtLeast (VCons (align (Fleet, Normal BalticSea) Germany) VNil) [])))))
        ]

-- 6.D.8. TEST CASE, FAILED CONVOY CAN NOT RECEIVE HOLD SUPPORT
--
-- If a convoy fails because of disruption of the convoy or when the right convoy orders are not given, then the army to be convoyed can not receive support in hold, since it still tried to move.
--
-- Austria: 
-- F Ionian Sea Hold
-- A Serbia Supports A Albania - Greece
-- A Albania - Greece
--
-- Turkey: 
-- A Greece - Naples
-- A Bulgaria Supports A Greece
--
-- There was a possible convoy from Greece to Naples, before the orders were made public (via the Ionian Sea). This means that the order of Greece to Naples should never be treated as illegal order and be changed in a hold order able to receive hold support (see also issue VI.A). Therefore, the support in Bulgaria fails and the army in Greece is dislodged by the army in Albania. 
sixD8 = (expectedResolution == testTypicalResolution expectedResolution) ~? "6.D.8"
  where

    expectedResolution = M.fromList [
          (Zone (Normal IonianSea), (align Fleet Austria, SomeResolved (MoveObject (Normal IonianSea), Nothing)))
        , (Zone (Normal Serbia), (align Army Austria, SomeResolved (SupportObject (Army, Normal Albania) (Normal Greece), Nothing)))
        , (Zone (Normal Albania), (align Army Austria, SomeResolved (MoveObject (Normal Greece), Nothing)))

        , (Zone (Normal Greece), (align Army Turkey, SomeResolved (MoveObject (Normal Naples), Just MoveNoConvoy)))
        , (Zone (Normal Bulgaria), (align Army Turkey, SomeResolved (SupportObject (Army, Normal Greece) (Normal Greece), Just SupportVoid)))
        ]

-- 6.D.9. TEST CASE, SUPPORT TO MOVE ON HOLDING UNIT NOT ALLOWED
--
-- A unit that is holding can not receive a support in moving.
--
-- Italy: 
-- A Venice - Trieste
-- A Tyrolia Supports A Venice - Trieste
--
-- Austria: 
-- A Albania Supports A Trieste - Serbia
-- A Trieste Hold
--
-- The support of the army in Albania fails and the army in Trieste is dislodged by the army from Venice. 
sixD9 = (expectedResolution == testTypicalResolution expectedResolution) ~? "6.D.9"
  where

    expectedResolution = M.fromList [
          (Zone (Normal Venice), (align Army Italy, SomeResolved (MoveObject (Normal Trieste), Nothing)))
        , (Zone (Normal Tyrolia), (align Army Italy, SomeResolved (SupportObject (Army, Normal Venice) (Normal Trieste), Nothing)))

        , (Zone (Normal Albania), (align Army Austria, SomeResolved (SupportObject (Army, Normal Trieste) (Normal Serbia), Just SupportVoid)))
        , (Zone (Normal Trieste), (align Army Austria, SomeResolved (MoveObject (Normal Trieste), Just (MoveOverpowered (AtLeast (VCons (align (Army, Normal Venice) Italy) VNil) [])))))
        ]

-- 6.D.10. TEST CASE, SELF DISLODGMENT PROHIBITED
--
-- A unit may not dislodge a unit of the same great power.
--
-- Germany: 
-- A Berlin Hold
-- F Kiel - Berlin
-- A Munich Supports F Kiel - Berlin
--
-- Move to Berlin fails. 
sixD10 = (expectedResolution == testTypicalResolution expectedResolution) ~? "6.D.10"
  where

    expectedResolution = M.fromList [
          (Zone (Normal Berlin), (align Army Germany, SomeResolved (MoveObject (Normal Berlin), Nothing)))
        -- NB this is not a friendly dislodge; it's actually bounced, because
        -- friendly support does not count against a holder.
        , (Zone (Normal Kiel), (align Fleet Germany, SomeResolved (MoveObject (Normal Berlin), Just (MoveBounced (AtLeast (VCons (align (Army, Normal Berlin) Germany) VNil) [])))))
        , (Zone (Normal Munich), (align Army Germany, SomeResolved (SupportObject (Fleet, Normal Kiel) (Normal Berlin), Nothing)))
        ]

-- 6.D.11. TEST CASE, NO SELF DISLODGMENT OF RETURNING UNIT
--
-- Idem.
--
-- Germany: 
-- A Berlin - Prussia
-- F Kiel - Berlin
-- A Munich Supports F Kiel - Berlin
--
-- Russia: 
-- A Warsaw - Prussia
--
-- Army in Berlin bounces, but is not dislodged by own unit. 
sixD11 = (expectedResolution == testTypicalResolution expectedResolution) ~? "6.D.11"
  where

    expectedResolution = M.fromList [
          (Zone (Normal Berlin), (align Army Germany, SomeResolved (MoveObject (Normal Prussia), Just (MoveBounced (AtLeast (VCons (align (Army, Normal Warsaw) Russia) VNil) [])))))
        -- NB does not fail due to a friendly dislodge! It's really a bounce,
        -- since its support from Germany cannot be tallied against the
        -- returning German unit.
        , (Zone (Normal Kiel), (align Fleet Germany, SomeResolved (MoveObject (Normal Berlin), Just (MoveBounced (AtLeast (VCons (align (Army, Normal Berlin) Germany) VNil) [])))))
        , (Zone (Normal Munich), (align Army Germany, SomeResolved (SupportObject (Fleet, Normal Kiel) (Normal Berlin), Nothing)))

        , (Zone (Normal Warsaw), (align Army Russia, SomeResolved (MoveObject (Normal Prussia), Just (MoveBounced (AtLeast (VCons (align (Army, Normal Berlin) Germany) VNil) [])))))
        ]

-- 6.D.12. TEST CASE, SUPPORTING A FOREIGN UNIT TO DISLODGE OWN UNIT PROHIBITED
--
-- You may not help another power in dislodging your own unit.
--
-- Austria: 
-- F Trieste Hold
-- A Vienna Supports A Venice - Trieste
--
-- Italy: 
-- A Venice - Trieste
--
-- No dislodgment of fleet in Trieste. 
sixD12 = (expectedResolution == testTypicalResolution expectedResolution) ~? "6.D.12"
  where

    expectedResolution = M.fromList [
          (Zone (Normal Trieste), (align Fleet Austria, SomeResolved (MoveObject (Normal Trieste), Nothing)))
        , (Zone (Normal Vienna), (align Army Austria, SomeResolved (SupportObject (Army, Normal Venice) (Normal Trieste), Nothing)))

        , (Zone (Normal Venice), (align Army Italy, SomeResolved (MoveObject (Normal Trieste), Just (MoveBounced (AtLeast (VCons (align (Fleet, Normal Trieste) Austria) VNil) [])))))
        ]

-- 6.D.13. TEST CASE, SUPPORTING A FOREIGN UNIT TO DISLODGE A RETURNING OWN UNIT PROHIBITED
--
-- Idem.
--
-- Austria: 
-- F Trieste - Adriatic Sea
-- A Vienna Supports A Venice - Trieste
--
-- Italy: 
-- A Venice - Trieste
-- F Apulia - Adriatic Sea
--
-- No dislodgment of fleet in Trieste. 
sixD13 = (expectedResolution == testTypicalResolution expectedResolution) ~? "6.D.13"
  where

    expectedResolution = M.fromList [
          (Zone (Normal Trieste), (align Fleet Austria, SomeResolved (MoveObject (Normal AdriaticSea), Just (MoveBounced (AtLeast (VCons (align (Fleet, Normal Apulia) Italy) VNil) [])))))
        , (Zone (Normal Vienna), (align Army Austria, SomeResolved (SupportObject (Army, Normal Venice) (Normal Trieste), Nothing)))

        , (Zone (Normal Venice), (align Army Italy, SomeResolved (MoveObject (Normal Trieste), Just (MoveBounced (AtLeast (VCons (align (Fleet, Normal Trieste) Austria) VNil) [])))))
        , (Zone (Normal Apulia), (align Fleet Italy, SomeResolved (MoveObject (Normal AdriaticSea), Just (MoveBounced (AtLeast (VCons (align (Fleet, Normal Trieste) Austria) VNil) [])))))
        ]

-- 6.D.14. TEST CASE, SUPPORTING A FOREIGN UNIT IS NOT ENOUGH TO PREVENT DISLODGEMENT
--
-- If a foreign unit has enough support to dislodge your unit, you may not prevent that dislodgement by supporting the attack.
--
-- Austria:
-- F Trieste Hold
-- A Vienna Supports A Venice - Trieste
--
-- Italy:
-- A Venice - Trieste
-- A Tyrolia Supports A Venice - Trieste
-- F Adriatic Sea Supports A Venice - Trieste
--
-- The fleet in Trieste is dislodged. 
sixD14 = (expectedResolution == testTypicalResolution expectedResolution) ~? "6.D.14"
  where

    expectedResolution = M.fromList [
          (Zone (Normal Trieste), (align Fleet Austria, SomeResolved (MoveObject (Normal Trieste), Just (MoveOverpowered (AtLeast (VCons (align (Army, Normal Venice) Italy) VNil) [])))))
        , (Zone (Normal Vienna), (align Army Austria, SomeResolved (SupportObject (Army, Normal Venice) (Normal Trieste), Nothing)))

        , (Zone (Normal Venice), (align Army Italy, SomeResolved (MoveObject (Normal Trieste), Nothing)))
        , (Zone (Normal Tyrolia), (align Army Italy, SomeResolved (SupportObject (Army, Normal Venice) (Normal Trieste), Nothing)))
        -- NB this final order really is superfluous, but nevertheless I follow
        -- the test cases as they're given.
        , (Zone (Normal AdriaticSea), (align Fleet Italy, SomeResolved (SupportObject (Army, Normal Venice) (Normal Trieste), Nothing)))
        ]

-- 6.D.15. TEST CASE, DEFENDER CAN NOT CUT SUPPORT FOR ATTACK ON ITSELF
--
-- A unit that is attacked by a supported unit can not prevent dislodgement by guessing which of the units will do the support.
--
-- Russia: 
-- F Constantinople Supports F Black Sea - Ankara
-- F Black Sea - Ankara
--
-- Turkey: 
-- F Ankara - Constantinople
--
-- The support of Constantinople is not cut and the fleet in Ankara is dislodged by the fleet in the Black Sea.
sixD15 = (expectedResolution == testTypicalResolution expectedResolution) ~? "6.D.15"
  where

    expectedResolution = M.fromList [
          (Zone (Normal Constantinople), (align Fleet Russia, SomeResolved (SupportObject (Fleet, Normal BlackSea) (Normal Ankara), Nothing)))
        , (Zone (Normal BlackSea), (align Fleet Russia, SomeResolved (MoveObject (Normal Ankara), Nothing)))

        , (Zone (Normal Ankara), (align Fleet Turkey, SomeResolved (MoveObject (Normal Constantinople), Just (MoveBounced (AtLeast (VCons (align (Fleet, Normal Constantinople) Russia) VNil) [])))))
        ]

-- 6.D.16. TEST CASE, CONVOYING A UNIT DISLODGING A UNIT OF SAME POWER IS ALLOWED
--
-- It is allowed to convoy a foreign unit that dislodges your own unit is allowed.
--
-- England: 
-- A London Hold
-- F North Sea Convoys A Belgium - London
--
-- France: 
-- F English Channel Supports A Belgium - London
-- A Belgium - London
--
-- The English army in London is dislodged by the French army coming from Belgium. 
sixD16 = (expectedResolution == testTypicalResolution expectedResolution) ~? "6.D.16"
  where

    expectedResolution = M.fromList [
          (Zone (Normal London), (align Army England, SomeResolved (MoveObject (Normal London), Just (MoveOverpowered (AtLeast (VCons (align (Army, Normal Belgium) France) VNil) [])))))
        , (Zone (Normal NorthSea), (align Fleet England, SomeResolved (ConvoyObject (Army, Normal Belgium) (Normal London), Nothing)))

        , (Zone (Normal EnglishChannel), (align Fleet France, SomeResolved (SupportObject (Army, Normal Belgium) (Normal London), Nothing)))
        , (Zone (Normal Belgium), (align Army France, SomeResolved (MoveObject (Normal London), Nothing)))
        ]

-- 6.D.17. TEST CASE, DISLODGEMENT CUTS SUPPORTS
--
-- The famous dislodge rule.
--
-- Russia: 
-- F Constantinople Supports F Black Sea - Ankara
-- F Black Sea - Ankara
--
-- Turkey: 
-- F Ankara - Constantinople
-- A Smyrna Supports F Ankara - Constantinople
-- A Armenia - Ankara
--
-- The Russian fleet in Constantinople is dislodged. This cuts the support to from Black Sea to Ankara. Black Sea will bounce with the army from Armenia. 
sixD17 = (expectedResolution == testTypicalResolution expectedResolution) ~? "6.D.17"
  where

    expectedResolution = M.fromList [
          (Zone (Normal Constantinople), (align Fleet Russia, SomeResolved (SupportObject (Fleet, Normal BlackSea) (Normal Ankara), Just (SupportDislodged (align (Fleet, Normal Ankara) Turkey)))))
        , (Zone (Normal BlackSea), (align Fleet Russia, SomeResolved (MoveObject (Normal Ankara), Just (MoveBounced (AtLeast (VCons (align (Army, Normal Armenia) Turkey) VNil) [])))))

        , (Zone (Normal Ankara), (align Fleet Turkey, SomeResolved (MoveObject (Normal Constantinople), Nothing)))
        , (Zone (Normal Smyrna), (align Army Turkey, SomeResolved (SupportObject (Fleet, Normal Ankara) (Normal Constantinople), Nothing)))
        , (Zone (Normal Armenia), (align Army Turkey, SomeResolved (MoveObject (Normal Ankara), Just (MoveBounced (AtLeast (VCons (align (Fleet, Normal BlackSea) Russia) VNil) [])))))
        ]

-- 6.D.18. TEST CASE, A SURVIVING UNIT WILL SUSTAIN SUPPORT
--
-- Idem. But now with an additional hold that prevents dislodgement.
--
-- Russia: 
-- F Constantinople Supports F Black Sea - Ankara
-- F Black Sea - Ankara
-- A Bulgaria Supports F Constantinople
--
-- Turkey: 
-- F Ankara - Constantinople
-- A Smyrna Supports F Ankara - Constantinople
-- A Armenia - Ankara
--
-- The Russian fleet in the Black Sea will dislodge the Turkish fleet in Ankara. 
sixD18 = (expectedResolution == testTypicalResolution expectedResolution) ~? "6.D.18"
  where

    expectedResolution = M.fromList [
          (Zone (Normal Constantinople), (align Fleet Russia, SomeResolved (SupportObject (Fleet, Normal BlackSea) (Normal Ankara), Nothing)))
        , (Zone (Normal BlackSea), (align Fleet Russia, SomeResolved (MoveObject (Normal Ankara), Nothing)))
        , (Zone (Normal Bulgaria), (align Army Russia, SomeResolved (SupportObject (Fleet, Normal Constantinople) (Normal Constantinople), Nothing)))

        , (Zone (Normal Ankara), (align Fleet Turkey, SomeResolved (MoveObject (Normal Constantinople), Just (MoveBounced (AtLeast (VCons (align (Fleet, Normal Constantinople) Russia) VNil) [])))))
        , (Zone (Normal Smyrna), (align Army Turkey, SomeResolved (SupportObject (Fleet, Normal Ankara) (Normal Constantinople), Nothing)))
        , (Zone (Normal Armenia), (align Army Turkey, SomeResolved (MoveObject (Normal Ankara), Just (MoveOverpowered (AtLeast (VCons (align (Fleet, Normal BlackSea) Russia) VNil) [])))))
        ]

-- 6.D.19. TEST CASE, EVEN WHEN SURVIVING IS IN ALTERNATIVE WAY
--
-- Now, the dislodgement is prevented because the supports comes from a Russian army:
--
-- Russia: 
-- F Constantinople Supports F Black Sea - Ankara
-- F Black Sea - Ankara
-- A Smyrna Supports F Ankara - Constantinople
--
-- Turkey: 
-- F Ankara - Constantinople
--
-- The Russian fleet in Constantinople is not dislodged, because one of the support is of Russian origin. The support from Black Sea to Ankara will sustain and the fleet in Ankara will be dislodged. 
sixD19 = (expectedResolution == testTypicalResolution expectedResolution) ~? "6.D.19"
  where

    expectedResolution = M.fromList [
          (Zone (Normal Constantinople), (align Fleet Russia, SomeResolved (SupportObject (Fleet, Normal BlackSea) (Normal Ankara), Nothing)))
        , (Zone (Normal BlackSea), (align Fleet Russia, SomeResolved (MoveObject (Normal Ankara), Nothing)))
        , (Zone (Normal Smyrna), (align Army Russia, SomeResolved (SupportObject (Fleet, Normal Ankara) (Normal Constantinople), Nothing)))

        , (Zone (Normal Ankara), (align Fleet Turkey, SomeResolved (MoveObject (Normal Constantinople), Just (MoveBounced (AtLeast (VCons (align (Fleet, Normal Constantinople) Russia) VNil) [])))))
        ]

-- 6.D.20. TEST CASE, UNIT CAN NOT CUT SUPPORT OF ITS OWN COUNTRY
--
-- Although this is not mentioned in all rulebooks, it is generally accepted that when a unit attacks another unit of the same Great Power, it will not cut support.
--
-- England: 
-- F London Supports F North Sea - English Channel
-- F North Sea - English Channel
-- A Yorkshire - London
--
-- France: 
-- F English Channel Hold
--
-- The army in York does not cut support. This means that the fleet in the English Channel is dislodged by the fleet in the North Sea. 
sixD20 = (expectedResolution == testTypicalResolution expectedResolution) ~? "6.D.20"
  where

    expectedResolution = M.fromList [
          (Zone (Normal London), (align Fleet England, SomeResolved (SupportObject (Fleet, Normal NorthSea) (Normal EnglishChannel), Nothing)))
        , (Zone (Normal NorthSea), (align Fleet England, SomeResolved (MoveObject (Normal EnglishChannel), Nothing)))
        , (Zone (Normal Yorkshire), (align Army England, SomeResolved (MoveObject (Normal London), Just (MoveBounced (AtLeast (VCons (align (Fleet, Normal London) England) VNil) [])))))

        , (Zone (Normal EnglishChannel), (align Fleet France, SomeResolved (MoveObject (Normal EnglishChannel), Just (MoveOverpowered (AtLeast (VCons (align (Fleet, Normal NorthSea) England) VNil) [])))))
        ]

-- 6.D.21. TEST CASE, DISLODGING DOES NOT CANCEL A SUPPORT CUT
--
-- Sometimes there is the question whether a dislodged moving unit does not cut support (similar to the dislodge rule). This is not the case.
--
-- Austria: 
-- F Trieste Hold
--  
-- Italy: 
-- A Venice - Trieste
-- A Tyrolia Supports A Venice - Trieste
--         
-- Germany: 
-- A Munich - Tyrolia
--
-- Russia: 
-- A Silesia - Munich
-- A Berlin Supports A Silesia - Munich
--
-- Although the German army is dislodged, it still cuts the Italian support. That means that the Austrian Fleet is not dislodged. 
sixD21 = (expectedResolution == testTypicalResolution expectedResolution) ~? "6.D.21"
  where

    expectedResolution = M.fromList [
          (Zone (Normal Trieste), (align Fleet Austria, SomeResolved (MoveObject (Normal Trieste), Nothing)))

        , (Zone (Normal Venice), (align Army Italy, SomeResolved (MoveObject (Normal Trieste), Just (MoveBounced (AtLeast (VCons (align (Fleet, Normal Trieste) Austria) VNil) [])))))
        , (Zone (Normal Tyrolia), (align Army Italy, SomeResolved (SupportObject (Army, Normal Venice) (Normal Trieste), Just (SupportCut (AtLeast (VCons (align (Army, Normal Munich) Germany) VNil) [])))))

        , (Zone (Normal Munich), (align Army Germany, SomeResolved (MoveObject (Normal Tyrolia), Just (MoveBounced (AtLeast (VCons (align (Army, Normal Tyrolia) Italy) VNil) [])))))

        , (Zone (Normal Silesia), (align Army Russia, SomeResolved (MoveObject (Normal Munich), Nothing)))
        , (Zone (Normal Berlin), (align Army Russia, SomeResolved (SupportObject (Army, Normal Silesia) (Normal Munich), Nothing)))
        ]

-- 6.D.22. TEST CASE, IMPOSSIBLE FLEET MOVE CAN NOT BE SUPPORTED
--
-- If a fleet tries moves to a land area it seems pointless to support the fleet, since the move will fail anyway. However, in such case, the support is also invalid for defense purposes.
--
-- Germany: 
-- F Kiel - Munich
-- A Burgundy Supports F Kiel - Munich
--
-- Russia: 
-- A Munich - Kiel
-- A Berlin Supports A Munich - Kiel
--
-- The German move from Kiel to Munich is illegal (fleets can not go to Munich). Therefore, the support from Burgundy fails and the Russian army in Munich will dislodge the fleet in Kiel. Note that the failing of the support is not explicitly mentioned in the rulebooks (the DPTG is more clear about this point). If you take the rulebooks very literally, you might conclude that the fleet in Munich is not dislodged, but this is an incorrect interpretation.
--
-- Note: this case does not test our resolver, but our verifier. It deals
-- with orders which makes no sense even in a vacuum, rather than with valid
-- orders which may fail depending upon the other orders given.
-- You can give these orders to the resolver, and there will be a standoff, but
-- that's not a failure of the resolver.
sixD22 = S.member SupportedCanDoMove validation ~? "6.D.22"
  where

    validation = analyze snd (S.singleton . fst) S.empty S.union (supportVOC Germany occupation) supportOrder
    supportOrder = Order ((Army, Normal Burgundy), SupportObject (Fleet, Normal Kiel) (Normal Munich))
    occupation =
          occupy (Normal Burgundy) (Just (align Army Germany))
        . occupy (Normal Kiel) (Just (align Fleet Germany))
        $ emptyOccupation

-- 6.D.23. TEST CASE, IMPOSSIBLE COAST MOVE CAN NOT BE SUPPORTED
--
-- Comparable with the previous test case, but now the fleet move is impossible for coastal reasons.
--
-- Italy: 
-- F Gulf of Lyon - Spain(sc)
-- F Western Mediterranean Supports F Gulf of Lyon - Spain(sc)
--
-- France: 
-- F Spain(nc) - Gulf of Lyon
-- F Marseilles Supports F Spain(nc) - Gulf of Lyon
--
-- The French move from Spain North Coast to Gulf of Lyon is illegal (wrong coast). Therefore, the support from Marseilles fails and the fleet in Spain is dislodged. 
--
-- Note: see 6.D.22; this one is also a test of the verifier, not resolver.
sixD23 = S.member SupportedCanDoMove validation ~? "6.D.23"
  where

    validation = analyze snd (S.singleton . fst) S.empty S.union (supportVOC France occupation) supportOrder
    supportOrder = Order ((Fleet, Normal Marseilles), SupportObject (Fleet, Special SpainNorth) (Normal GulfOfLyon))
    occupation =
          occupy (Normal Marseilles) (Just (align Fleet France))
        . occupy (Special SpainNorth) (Just (align Fleet France))
        $ emptyOccupation

-- 6.D.24. TEST CASE, IMPOSSIBLE ARMY MOVE CAN NOT BE SUPPORTED
--
-- Comparable with the previous test case, but now an army tries to move into sea and the support is used in a beleaguered garrison.
--
-- France: 
-- A Marseilles - Gulf of Lyon
-- F Spain(sc) Supports A Marseilles - Gulf of Lyon
--
-- Italy: 
-- F Gulf of Lyon Hold
--
-- Turkey: 
-- F Tyrrhenian Sea Supports F Western Mediterranean - Gulf of Lyon
-- F Western Mediterranean - Gulf of Lyon
--
-- The French move from Marseilles to Gulf of Lyon is illegal (an army can not go to sea). Therefore, the support from Spain fails and there is no beleaguered garrison. The fleet in the Gulf of Lyon is dislodged by the Turkish fleet in the Western Mediterranean. 
--
-- Note: see 6.D.23, 6.D.22; this is also a test of the verifier, not the
-- resolver.
sixD24 = S.member SupportedCanDoMove validation ~? "6.D.24"
  where

    validation = analyze snd (S.singleton . fst) S.empty S.union (supportVOC France occupation) supportOrder
    supportOrder = Order ((Fleet, Special SpainSouth), SupportObject (Army, Normal Marseilles) (Normal GulfOfLyon))
    occupation =
          occupy (Normal Marseilles) (Just (align Army France))
        . occupy (Special SpainSouth) (Just (align Fleet France))
        $ emptyOccupation

-- 6.D.25. TEST CASE, FAILING HOLD SUPPORT CAN BE SUPPORTED
--
-- If an adjudicator fails on one of the previous three test cases, then the bug should be removed with care. A failing move can not be supported, but a failing hold support, because of some preconditions (unmatching order) can still be supported.
--
-- Germany: 
-- A Berlin Supports A Prussia
-- F Kiel Supports A Berlin
--
-- Russia: 
-- F Baltic Sea Supports A Prussia - Berlin
-- A Prussia - Berlin
--
-- Although the support of Berlin on Prussia fails (because of unmatching orders), the support of Kiel on Berlin is still valid. So, Berlin will not be dislodged. 
--
-- Note: this one mentions the previous three cases, which test the verifier;
-- yet, this one tests the resolver! All orders in this case are indeed
-- valid.
sixD25 = (expectedResolution == testTypicalResolution expectedResolution) ~? "6.D.25"
  where

    expectedResolution = M.fromList [
          (Zone (Normal Berlin), (align Army Germany, SomeResolved (SupportObject (Army, Normal Prussia) (Normal Prussia), Just SupportVoid)))
        , (Zone (Normal Kiel), (align Fleet Germany, SomeResolved (SupportObject (Army, Normal Berlin) (Normal Berlin), Nothing)))

        , (Zone (Normal BalticSea), (align Fleet Russia, SomeResolved (SupportObject (Army, Normal Prussia) (Normal Berlin), Nothing)))
        , (Zone (Normal Prussia), (align Army Russia, SomeResolved (MoveObject (Normal Berlin), Just (MoveBounced (AtLeast (VCons (align (Army, Normal Berlin) Germany) VNil) [])))))
        ]

-- 6.D.26. TEST CASE, FAILING MOVE SUPPORT CAN BE SUPPORTED
--
-- Similar as the previous test case, but now with an unmatched support to move.
--
-- Germany: 
-- A Berlin Supports A Prussia - Silesia
-- F Kiel Supports A Berlin
--
-- Russia: 
-- F Baltic Sea Supports A Prussia - Berlin
-- A Prussia - Berlin
--
-- Again, Berlin will not be dislodged. 
sixD26 = (expectedResolution == testTypicalResolution expectedResolution) ~? "6.D.26"
  where

    expectedResolution = M.fromList [
          (Zone (Normal Berlin), (align Army Germany, SomeResolved (SupportObject (Army, Normal Prussia) (Normal Silesia), Just SupportVoid)))
        , (Zone (Normal Kiel), (align Fleet Germany, SomeResolved (SupportObject (Army, Normal Berlin) (Normal Berlin), Nothing)))

        , (Zone (Normal BalticSea), (align Fleet Russia, SomeResolved (SupportObject (Army, Normal Prussia) (Normal Berlin), Nothing)))
        , (Zone (Normal Prussia), (align Army Russia, SomeResolved (MoveObject (Normal Berlin), Just (MoveBounced (AtLeast (VCons (align (Army, Normal Berlin) Germany) VNil) [])))))
        ]

-- 6.D.27. TEST CASE, FAILING CONVOY CAN BE SUPPORTED
--
-- Similar as the previous test case, but now with an unmatched convoy.
--
-- England: 
-- F Sweden - Baltic Sea
-- F Denmark Supports F Sweden - Baltic Sea
--
-- Germany: 
-- A Berlin Hold
--
-- Russia: 
-- F Baltic Sea Convoys A Berlin - Livonia
-- F Prussia Supports F Baltic Sea
--
-- The convoy order in the Baltic Sea is unmatched and fails. However, the support of Prussia on the Baltic Sea is still valid and the fleet in the Baltic Sea is not dislodged. 
sixD27 = (expectedResolution == testTypicalResolution expectedResolution) ~? "6.D.27"
  where

    expectedResolution = M.fromList [
          (Zone (Normal Sweden), (align Fleet England, SomeResolved (MoveObject (Normal BalticSea), Just (MoveBounced (AtLeast (VCons (align (Fleet, Normal BalticSea) Russia) VNil) [])))))
        , (Zone (Normal Denmark), (align Fleet England, SomeResolved (SupportObject (Fleet, Normal Sweden) (Normal BalticSea), Nothing)))

        , (Zone (Normal Berlin), (align Army Germany, SomeResolved (MoveObject (Normal Berlin), Nothing)))

        , (Zone (Normal BalticSea), (align Fleet Russia, SomeResolved (ConvoyObject (Army, Normal Berlin) (Normal Livonia), Just ConvoyVoid)))
        , (Zone (Normal Prussia), (align Fleet Russia, SomeResolved (SupportObject (Fleet, Normal BalticSea) (Normal BalticSea), Nothing)))
        ]

-- 6.D.28. TEST CASE, IMPOSSIBLE MOVE AND SUPPORT
--
-- If a move is impossible then it can be treated as "illegal", which makes a hold support possible.
--
-- Austria: 
-- A Budapest Supports F Rumania
--
-- Russia: 
-- F Rumania - Holland
--
-- Turkey: 
-- F Black Sea - Rumania
-- A Bulgaria Supports F Black Sea - Rumania
--
-- The move of the Russian fleet is impossible. But the question is, whether it is "illegal" (see issue 4.E.1). If the move is "illegal" it must be ignored and that makes the hold support of the army in Budapest valid and the fleet in Rumania will not be dislodged. 
--
-- Note: here we go way off track and just guarantee that the invalid move
-- is indeed recognized as invalid, without doing any resolution. The resolver,
-- in this case (with the invalid move order) would recognize the Austrian
-- support as void and the Russian fleet would successfully move from Rumania
-- to Holland. That's not a bug, though; the resolver does not demand valid
-- orders.
sixD28 = S.member MoveReachable validation ~? "6.D.28"
  where

    validation = analyze snd (S.singleton . fst) S.empty S.union (moveVOC Russia occupation) moveOrder
    moveOrder = Order ((Fleet, Normal Rumania), MoveObject (Normal Holland))
    occupation =
          occupy (Normal Rumania) (Just (align Fleet Russia))
        $ emptyOccupation

-- 6.D.29. TEST CASE, MOVE TO IMPOSSIBLE COAST AND SUPPORT
--
-- Similar to the previous test case, but now the move can be "illegal" because of the wrong coast.
--
-- Austria: 
-- A Budapest Supports F Rumania
--
-- Russia: 
-- F Rumania - Bulgaria(sc)
--
-- Turkey: 
-- F Black Sea - Rumania
-- A Bulgaria Supports F Black Sea - Rumania
--
-- Again the move of the Russian fleet is impossible. However, some people might correct the coast (see issue 4.B.3). If the coast is not corrected, again the question is whether it is "illegal" (see issue 4.E.1). If the move is "illegal" it must be ignored and that makes the hold support of the army in Budapest valid and the fleet in Rumania will not be dislodged. 
--
-- Note: again we decimate a test case, ignoring resolutions and testing
-- validations instead.
sixD29 = S.member MoveReachable validation ~? "6.D.29"
  where

    validation = analyze snd (S.singleton . fst) S.empty S.union (moveVOC Russia occupation) moveOrder
    moveOrder = Order ((Fleet, Normal Rumania), MoveObject (Special BulgariaSouth))
    occupation =
          occupy (Normal Rumania) (Just (align Fleet Russia))
        $ emptyOccupation

-- 6.D.30. TEST CASE, MOVE WITHOUT COAST AND SUPPORT
--
-- Similar to the previous test case, but now the move can be "illegal" because of missing coast.
--
-- Italy: 
-- F Aegean Sea Supports F Constantinople
--
-- Russia: 
-- F Constantinople - Bulgaria
--
-- Turkey: 
-- F Black Sea - Constantinople
-- A Bulgaria Supports F Black Sea - Constantinople
--
-- Again the order to the Russian fleet is with problems, because it does not specify the coast, while both coasts of Bulgaria are possible. If no default coast is taken (see issue 4.B.1), then also here it must be decided whether the order is "illegal" (see issue 4.E.1). If the move is "illegal" it must be ignored and that makes the hold support of the fleet in the Aegean Sea valid and the Russian fleet will not be dislodged. 
sixD30 = S.member MoveUnitCanOccupy validation ~? "6.D.30"
  where

    validation = analyze snd (S.singleton . fst) S.empty S.union (moveVOC Russia occupation) moveOrder
    moveOrder = Order ((Fleet, Normal Rumania), MoveObject (Normal Bulgaria))
    occupation =
          occupy (Normal Rumania) (Just (align Fleet Russia))
        $ emptyOccupation

-- 6.D.31. TEST CASE, A TRICKY IMPOSSIBLE SUPPORT
--
-- A support order can be impossible for complex reasons.
--
-- Austria: 
-- A Rumania - Armenia
--
-- Turkey: 
-- F Black Sea Supports A Rumania - Armenia
--
-- Although the army in Rumania can move to Armenia and the fleet in the Black Sea can also go to Armenia, the support is still not possible. The reason is that the only possible convoy is through the Black Sea and a fleet can not convoy and support at the same time.
--
-- This is relevant for computer programs that show only the possible orders. In the list of possible orders, the support as given to the fleet in the Black Sea, should not be listed. Furthermore, if the fleet in the Black Sea gets a second order, then this may fail, because of double orders (although it can also be ruled differently, see issue 4.D.3). However, when the support order is considered "illegal" (see issue 4.E.1), then this impossible support must be ignored and the second order must be carried out. 
--
-- Note: I take issue with this one. In my opinion the support is just fine.
-- Sure, the move it supports will always fail due to lack of a convoy route
-- (assuming all convoy orders are valid), but so what?

-- 6.D.32. TEST CASE, A MISSING FLEET
--
-- The previous test cases contained an order that was impossible even when some other pieces on the board where changed. In this test case, the order is impossible, but only for that situation.
--
-- England: 
-- F Edinburgh Supports A Liverpool - Yorkshire
-- A Liverpool - Yorkshire
--
-- France: 
-- F London Supports A Yorkshire
--
-- Germany: 
-- A Yorkshire - Holland
--
-- The German order to Yorkshire can not be executed, because there is no fleet in the North Sea. In other situations (where there is a fleet in the North Sea), the exact same order would be possible. It should be determined whether this is "illegal" (see issue 4.E.1) or not. If it is illegal, then the order should be ignored and the support of the French fleet in London succeeds. This means that the army in Yorkshire is not dislodged. 
--
-- Note: I take issue with this one as well. I believe everything here is
-- valid. The German move will fail due to lack of a convoy and then the
-- English move will dislodge the German army which returns to Yorkshire.

-- 6.D.33. TEST CASE, UNWANTED SUPPORT ALLOWED
--
-- A self stand-off can be broken by an unwanted support.
--
-- Austria: 
-- A Serbia - Budapest
-- A Vienna - Budapest
--
-- Russia: 
-- A Galicia Supports A Serbia - Budapest
--
-- Turkey: 
-- A Bulgaria - Serbia
--
-- Due to the Russian support, the army in Serbia advances to Budapest. This enables Turkey to capture Serbia with the army in Bulgaria. 
sixD33 = (expectedResolution == testTypicalResolution expectedResolution) ~? "6.D.33"
  where

    expectedResolution = M.fromList [
          (Zone (Normal Serbia), (align Army Austria, SomeResolved (MoveObject (Normal Budapest), Nothing)))
        , (Zone (Normal Vienna), (align Army Austria, SomeResolved (MoveObject (Normal Budapest), Just (MoveOverpowered (AtLeast (VCons (align (Army, Normal Serbia) Austria) VNil) [])))))

        , (Zone (Normal Galicia), (align Army Russia, SomeResolved (SupportObject (Army, Normal Serbia) (Normal Budapest), Nothing)))

        , (Zone (Normal Bulgaria), (align Army Turkey, SomeResolved (MoveObject (Normal Serbia), Nothing)))
        ]

-- 6.D.34. TEST CASE, SUPPORT TARGETING OWN AREA NOT ALLOWED
-- Support targeting the area where the supporting unit is standing, is illegal.
--
-- Germany:
-- A Berlin - Prussia
-- A Silesia Supports A Berlin - Prussia
-- F Baltic Sea Supports A Berlin - Prussia
--
-- Italy:
-- A Prussia Supports Livonia - Prussia
--
-- Russia:
-- A Warsaw Supports A Livonia - Prussia
-- A Livonia - Prussia
--
-- Russia and Italy wanted to get rid of the Italian army in Prussia (to build an Italian fleet somewhere else). However, they didn't want a possible German attack on Prussia to succeed. They invented this odd order of Italy. It was intended that the attack of the army in Livonia would have strength three, so it would be capable to prevent the possible German attack to succeed. However, the order of Italy is illegal, because a unit may only support to an area where the unit can go by itself. A unit can't go to the area it is already standing, so the Italian order is illegal and the German move from Berlin succeeds. Even if it would be legal, the German move from Berlin would still succeed, because the support of Prussia is cut by Livonia and Berlin. 
--
-- Note: this is validation, not resolution, since the resolver does not make
-- an effort to default bogus orders.
sixD34 = S.member SupporterAdjacent validation ~? "6.D.34"
  where

    validation = analyze snd (S.singleton . fst) S.empty S.union (supportVOC Italy occupation) supportOrder
    supportOrder = Order ((Army, Normal Prussia), SupportObject (Army, Normal Livonia) (Normal Prussia))
    occupation =
          occupy (Normal Prussia) (Just (align Army Italy))
        -- Must put the russian army in the occupation, else the support order
        -- will fail for other reasons.
        . occupy (Normal Livonia) (Just (align Army Russia))
        $ emptyOccupation

-- 6.E.1. TEST CASE, DISLODGED UNIT HAS NO EFFECT ON ATTACKERS AREA
--
-- An army can follow.
--
-- Germany: 
-- A Berlin - Prussia
-- F Kiel - Berlin
-- A Silesia Supports A Berlin - Prussia
--
-- Russia: 
-- A Prussia - Berlin
--
-- The army in Kiel will move to Berlin. 
sixE1 = (expectedResolution == testTypicalResolution expectedResolution) ~? "6.E.1"
--sixE1 = (expectedResolution, testTypicalResolution expectedResolution)
  where

    expectedResolution = M.fromList [
          (Zone (Normal Berlin), (align Army Germany, SomeResolved (MoveObject (Normal Prussia), Nothing)))
        , (Zone (Normal Kiel), (align Fleet Germany, SomeResolved (MoveObject (Normal Berlin), Nothing)))
        , (Zone (Normal Silesia), (align Army Germany, SomeResolved (SupportObject (Army, Normal Berlin) (Normal Prussia), Nothing)))

        , (Zone (Normal Prussia), (align Army Russia, SomeResolved (MoveObject (Normal Berlin), Just (MoveOverpowered (AtLeast (VCons (align (Army, Normal Berlin) Germany) VNil) [])))))
        ]

-- 6.E.2. TEST CASE, NO SELF DISLODGEMENT IN HEAD TO HEAD BATTLE
--
-- Self dislodgement is not allowed. This also counts for head to head battles.
--
-- Germany: 
-- A Berlin - Kiel
-- F Kiel - Berlin
-- A Munich Supports A Berlin - Kiel
--
-- No unit will move. 
sixE2 = (expectedResolution == testTypicalResolution expectedResolution) ~? "6.E.2"
  where

    expectedResolution = M.fromList [
          -- NB these fail with 2-cycle because the friendly support does
          -- not count.
          (Zone (Normal Berlin), (align Army Germany, SomeResolved (MoveObject (Normal Kiel), Just (MoveBounced (AtLeast (VCons (align (Fleet, Normal Kiel) Germany) VNil) [])))))
        , (Zone (Normal Kiel), (align Fleet Germany, SomeResolved (MoveObject (Normal Berlin), Just (MoveBounced (AtLeast (VCons (align (Army, Normal Berlin) Germany) VNil) [])))))
        , (Zone (Normal Munich), (align Army Germany, SomeResolved (SupportObject (Army, Normal Berlin) (Normal Kiel), Nothing)))
        ]

-- 6.E.3. TEST CASE, NO HELP IN DISLODGING OWN UNIT
--
-- To help a foreign power to dislodge own unit in head to head battle is not possible.
--
-- Germany: 
-- A Berlin - Kiel
-- A Munich Supports F Kiel - Berlin
--
-- England: 
-- F Kiel - Berlin
--
-- No unit will move. 
sixE3 = (expectedResolution == testTypicalResolution expectedResolution) ~? "6.E.3"
  where

    expectedResolution = M.fromList [
          (Zone (Normal Berlin), (align Army Germany, SomeResolved (MoveObject (Normal Kiel), Just (MoveBounced (AtLeast (VCons (align (Fleet, Normal Kiel) England) VNil) [])))))
        , (Zone (Normal Munich), (align Army Germany, SomeResolved (SupportObject (Fleet, Normal Kiel) (Normal Berlin), Nothing)))

        , (Zone (Normal Kiel), (align Fleet England, SomeResolved (MoveObject (Normal Berlin), Just (MoveBounced (AtLeast (VCons (align (Army, Normal Berlin) Germany) VNil) [])))))
        ]

-- 6.E.4. TEST CASE, NON-DISLODGED LOSER HAS STILL EFFECT
--
-- If in an unbalanced head to head battle the loser is not dislodged, it has still effect on the area of the attacker.
--
-- Germany: 
-- F Holland - North Sea
-- F Helgoland Bight Supports F Holland - North Sea
-- F Skagerrak Supports F Holland - North Sea
--
-- France: 
-- F North Sea - Holland
-- F Belgium Supports F North Sea - Holland
--
-- England: 
-- F Edinburgh Supports F Norwegian Sea - North Sea
-- F Yorkshire Supports F Norwegian Sea - North Sea
-- F Norwegian Sea - North Sea
--
-- Austria: 
-- A Kiel Supports A Ruhr - Holland
-- A Ruhr - Holland
--
-- The French fleet in the North Sea is not dislodged due to the beleaguered garrison. Therefore, the Austrian army in Ruhr will not move to Holland. 
sixE4 = (expectedResolution == testTypicalResolution expectedResolution) ~? "6.E.4"
--sixE4 = (expectedResolution, testTypicalResolution expectedResolution)
  where

    expectedResolution = M.fromList [
          (Zone (Normal Holland), (align Fleet Germany, SomeResolved (MoveObject (Normal NorthSea), Just (MoveBounced (AtLeast (VCons (align (Fleet, Normal NorwegianSea) England) VNil) [])))))
        , (Zone (Normal HeligolandBight), (align Fleet Germany, SomeResolved (SupportObject (Fleet, Normal Holland) (Normal NorthSea), Nothing)))
        , (Zone (Normal Skagerrak), (align Fleet Germany, SomeResolved (SupportObject (Fleet, Normal Holland) (Normal NorthSea), Nothing)))

        -- This is overpowered by the german move in the head-to-head. You
        -- could also say it bounced off the Austrian army, but the resolver
        -- always gives an overpowered over a bounced because it is, to put it
        -- vaguely, more severe than a bounce, i.e. a more compelling reason
        -- for failure of the move.
        , (Zone (Normal NorthSea), (align Fleet France, SomeResolved (MoveObject (Normal Holland), Just (MoveOverpowered (AtLeast (VCons (align (Fleet, Normal Holland) Germany) VNil) [])))))
        , (Zone (Normal Belgium), (align Fleet France, SomeResolved (SupportObject (Fleet, Normal NorthSea) (Normal Holland), Nothing)))

        , (Zone (Normal Edinburgh), (align Fleet England, SomeResolved (SupportObject (Fleet, Normal NorwegianSea) (Normal NorthSea), Nothing)))
        , (Zone (Normal Yorkshire), (align Fleet England, SomeResolved (SupportObject (Fleet, Normal NorwegianSea) (Normal NorthSea), Nothing)))
        , (Zone (Normal NorwegianSea), (align Fleet England, SomeResolved (MoveObject (Normal NorthSea), Just (MoveBounced (AtLeast (VCons (align (Fleet, Normal Holland) Germany) VNil) [])))))

        , (Zone (Normal Kiel), (align Army Austria, SomeResolved (SupportObject (Army, Normal Ruhr) (Normal Holland), Nothing)))
        , (Zone (Normal Ruhr), (align Army Austria, SomeResolved (MoveObject (Normal Holland), Just (MoveBounced (AtLeast (VCons (align (Fleet, Normal NorthSea) France) VNil) [])))))
        ]

-- 6.E.5. TEST CASE, LOSER DISLODGED BY ANOTHER ARMY HAS STILL EFFECT
--
-- If in an unbalanced head to head battle the loser is dislodged by a unit not part of the head to head battle, the loser has still effect on the place of the winner of the head to head battle.
--
-- Germany: 
-- F Holland - North Sea
-- F Helgoland Bight Supports F Holland - North Sea
-- F Skagerrak Supports F Holland - North Sea
--
-- France: 
-- F North Sea - Holland
-- F Belgium Supports F North Sea - Holland
--
-- England: 
-- F Edinburgh Supports F Norwegian Sea - North Sea
-- F Yorkshire Supports F Norwegian Sea - North Sea
-- F Norwegian Sea - North Sea
-- F London Supports F Norwegian Sea - North Sea
--
-- Austria: 
-- A Kiel Supports A Ruhr - Holland
-- A Ruhr - Holland
--
-- The French fleet in the North Sea is dislodged but not by the German fleet in Holland. Therefore, the French fleet can still prevent that the Austrian army in Ruhr will move to Holland. So, the Austrian move in Ruhr fails and the German fleet in Holland is not dislodged. 
sixE5 = (expectedResolution == testTypicalResolution expectedResolution) ~? "6.E.5"
  where

    expectedResolution = M.fromList [
          (Zone (Normal Holland), (align Fleet Germany, SomeResolved (MoveObject (Normal NorthSea), Just (MoveOverpowered (AtLeast (VCons (align (Fleet, Normal NorwegianSea) England) VNil) [])))))
        , (Zone (Normal HeligolandBight), (align Fleet Germany, SomeResolved (SupportObject (Fleet, Normal Holland) (Normal NorthSea), Nothing)))
        , (Zone (Normal Skagerrak), (align Fleet Germany, SomeResolved (SupportObject (Fleet, Normal Holland) (Normal NorthSea), Nothing)))

        , (Zone (Normal NorthSea), (align Fleet France, SomeResolved (MoveObject (Normal Holland), Just (MoveOverpowered (AtLeast (VCons (align (Fleet, Normal Holland) Germany) VNil) [])))))
        , (Zone (Normal Belgium), (align Fleet France, SomeResolved (SupportObject (Fleet, Normal NorthSea) (Normal Holland), Nothing)))

        , (Zone (Normal Edinburgh), (align Fleet England, SomeResolved (SupportObject (Fleet, Normal NorwegianSea) (Normal NorthSea), Nothing)))
        , (Zone (Normal Yorkshire), (align Fleet England, SomeResolved (SupportObject (Fleet, Normal NorwegianSea) (Normal NorthSea), Nothing)))
        , (Zone (Normal NorwegianSea), (align Fleet England, SomeResolved (MoveObject (Normal NorthSea), Nothing)))
        , (Zone (Normal London), (align Fleet England, SomeResolved (SupportObject (Fleet, Normal NorwegianSea) (Normal NorthSea), Nothing)))

        , (Zone (Normal Kiel), (align Army Austria, SomeResolved (SupportObject (Army, Normal Ruhr) (Normal Holland), Nothing)))
        , (Zone (Normal Ruhr), (align Army Austria, SomeResolved (MoveObject (Normal Holland), Just (MoveBounced (AtLeast (VCons (align (Fleet, Normal NorthSea) France) VNil) [])))))
        ]

-- 6.E.6. TEST CASE, NOT DISLODGE BECAUSE OF OWN SUPPORT HAS STILL EFFECT
--
-- If in an unbalanced head to head battle the loser is not dislodged because the winner had help of a unit of the loser, the loser has still effect on the area of the winner.
--
-- Germany: 
-- F Holland - North Sea
-- F Helgoland Bight Supports F Holland - North Sea
--
-- France: 
-- F North Sea - Holland
-- F Belgium Supports F North Sea - Holland
-- F English Channel Supports F Holland - North Sea
--
-- Austria: 
-- A Kiel Supports A Ruhr - Holland
-- A Ruhr - Holland
--
-- Although the German force from Holland to North Sea is one larger than the French force from North Sea to Holland, the French fleet in the North Sea is not dislodged, because one of the supports on the German movement is French. Therefore, the Austrian army in Ruhr will not move to Holland. 
sixE6 = (expectedResolution == testTypicalResolution expectedResolution) ~? "6.E.6"
  where

    expectedResolution = M.fromList [
          (Zone (Normal Holland), (align Fleet Germany, SomeResolved (MoveObject (Normal NorthSea), Just (MoveBounced (AtLeast (VCons (align (Fleet, Normal NorthSea) France) VNil) [])))))
        , (Zone (Normal HeligolandBight), (align Fleet Germany, SomeResolved (SupportObject (Fleet, Normal Holland) (Normal NorthSea), Nothing)))

        , (Zone (Normal NorthSea), (align Fleet France, SomeResolved (MoveObject (Normal Holland), Just (MoveBounced (AtLeast (VCons (align (Army, Normal Ruhr) Austria) VNil) [])))))
        , (Zone (Normal Belgium), (align Fleet France, SomeResolved (SupportObject (Fleet, Normal NorthSea) (Normal Holland), Nothing)))
        , (Zone (Normal EnglishChannel), (align Fleet France, SomeResolved (SupportObject (Fleet, Normal Holland) (Normal NorthSea), Nothing)))

        , (Zone (Normal Kiel), (align Army Austria, SomeResolved (SupportObject (Army, Normal Ruhr) (Normal Holland), Nothing)))
        , (Zone (Normal Ruhr), (align Army Austria, SomeResolved (MoveObject (Normal Holland), Just (MoveBounced (AtLeast (VCons (align (Fleet, Normal NorthSea) France) VNil) [])))))
        ]

-- 6.E.7. TEST CASE, NO SELF DISLODGEMENT WITH BELEAGUERED GARRISON
--
-- An attempt to self dislodgement can be combined with a beleaguered garrison. Such self dislodgment is still not possible.
--
-- England: 
-- F North Sea Hold
-- F Yorkshire Supports F Norway - North Sea
--
-- Germany: 
-- F Holland Supports F Helgoland Bight - North Sea
-- F Helgoland Bight - North Sea
--
-- Russia: 
-- F Skagerrak Supports F Norway - North Sea
-- F Norway - North Sea
--
-- Although the Russians beat the German attack (with the support of Yorkshire) and the two Russian fleets are enough to dislodge the fleet in the North Sea, the fleet in the North Sea is not dislodged, since it would not be dislodged if the English fleet in Yorkshire would not give support. According to the DPTG the fleet in the North Sea would be dislodged. The DPTG is incorrect in this case. 
--
-- Note: this showed a bug in the resolver! When judging a move with stationary
-- incumbant, we compared foreign supports against the incumbant, but did not
-- look at the competing moves! We must to ask, in this case, is this move
-- still a dominator if we remove the stationary unit's friendly supports?
-- Same goes for the case of a returning unit!
sixE7 = (expectedResolution == testTypicalResolution expectedResolution) ~? "6.E.7"
  where

    expectedResolution = M.fromList [
          (Zone (Normal NorthSea), (align Fleet England, SomeResolved (MoveObject (Normal NorthSea), Nothing)))
        , (Zone (Normal Yorkshire), (align Fleet England, SomeResolved (SupportObject (Fleet, Normal Norway) (Normal NorthSea), Nothing)))

        , (Zone (Normal Holland), (align Fleet Germany, SomeResolved (SupportObject (Fleet, Normal HeligolandBight) (Normal NorthSea), Nothing)))
        , (Zone (Normal HeligolandBight), (align Fleet Germany, SomeResolved (MoveObject (Normal NorthSea), Just (MoveOverpowered (AtLeast (VCons (align (Fleet, Normal Norway) Russia) VNil) [])))))

        , (Zone (Normal Skagerrak), (align Fleet Russia, SomeResolved (SupportObject (Fleet, Normal Norway) (Normal NorthSea), Nothing)))
        , (Zone (Normal Norway), (align Fleet Russia, SomeResolved (MoveObject (Normal NorthSea), Just (MoveBounced (AtLeast (VCons (align (Fleet, Normal HeligolandBight) Germany) VNil) [])))))
        ]

-- 6.E.8. TEST CASE, NO SELF DISLODGEMENT WITH BELEAGUERED GARRISON AND HEAD TO HEAD BATTLE
--
-- Similar to the previous test case, but now the beleaguered fleet is also engaged in a head to head battle.
--
-- England: 
-- F North Sea - Norway
-- F Yorkshire Supports F Norway - North Sea
--
-- Germany: 
-- F Holland Supports F Helgoland Bight - North Sea
-- F Helgoland Bight - North Sea
--
-- Russia: 
-- F Skagerrak Supports F Norway - North Sea
-- F Norway - North Sea
--
-- Again, none of the fleets move. 
sixE8 = (expectedResolution == testTypicalResolution expectedResolution) ~? "6.E.8"
  where

    expectedResolution = M.fromList [
        -- English move is overpowered by the Russian move, since it has more
        -- support even without the English support.
          (Zone (Normal NorthSea), (align Fleet England, SomeResolved (MoveObject (Normal Norway), Just (MoveOverpowered (AtLeast (VCons (align (Fleet, Normal Norway) Russia) VNil) [])))))
        , (Zone (Normal Yorkshire), (align Fleet England, SomeResolved (SupportObject (Fleet, Normal Norway) (Normal NorthSea), Nothing)))

        , (Zone (Normal Holland), (align Fleet Germany, SomeResolved (SupportObject (Fleet, Normal HeligolandBight) (Normal NorthSea), Nothing)))
        -- German move is overpowered due to English + Russian support.
        , (Zone (Normal HeligolandBight), (align Fleet Germany, SomeResolved (MoveObject (Normal NorthSea), Just (MoveOverpowered (AtLeast (VCons (align (Fleet, Normal Norway) Russia) VNil) [])))))

        , (Zone (Normal Skagerrak), (align Fleet Russia, SomeResolved (SupportObject (Fleet, Normal Norway) (Normal NorthSea), Nothing)))
        -- Russian move bounces against the German fleet (English support is
        -- cast out here).
        , (Zone (Normal Norway), (align Fleet Russia, SomeResolved (MoveObject (Normal NorthSea), Just (MoveBounced (AtLeast (VCons (align (Fleet, Normal HeligolandBight) Germany) VNil) [])))))
        ]

-- 6.E.9. TEST CASE, ALMOST SELF DISLODGEMENT WITH BELEAGUERED GARRISON
--
-- Similar to the previous test case, but now the beleaguered fleet is moving away.
--
-- England: 
-- F North Sea - Norwegian Sea
-- F Yorkshire Supports F Norway - North Sea
--
-- Germany: 
-- F Holland Supports F Helgoland Bight - North Sea
-- F Helgoland Bight - North Sea
--
-- Russia: 
-- F Skagerrak Supports F Norway - North Sea
-- F Norway - North Sea
--
-- Both the fleet in the North Sea and the fleet in Norway move. 
sixE9 = (expectedResolution == testTypicalResolution expectedResolution) ~? "6.E.9"
  where

    expectedResolution = M.fromList [
          (Zone (Normal NorthSea), (align Fleet England, SomeResolved (MoveObject (Normal NorwegianSea), Nothing)))
        , (Zone (Normal Yorkshire), (align Fleet England, SomeResolved (SupportObject (Fleet, Normal Norway) (Normal NorthSea), Nothing)))

        , (Zone (Normal Holland), (align Fleet Germany, SomeResolved (SupportObject (Fleet, Normal HeligolandBight) (Normal NorthSea), Nothing)))
        , (Zone (Normal HeligolandBight), (align Fleet Germany, SomeResolved (MoveObject (Normal NorthSea), Just (MoveOverpowered (AtLeast (VCons (align (Fleet, Normal Norway) Russia) VNil) [])))))

        , (Zone (Normal Skagerrak), (align Fleet Russia, SomeResolved (SupportObject (Fleet, Normal Norway) (Normal NorthSea), Nothing)))
        , (Zone (Normal Norway), (align Fleet Russia, SomeResolved (MoveObject (Normal NorthSea), Nothing)))
        ] 

-- 6.E.10. TEST CASE, ALMOST CIRCULAR MOVEMENT WITH NO SELF DISLODGEMENT WITH BELEAGUERED GARRISON
--
-- Similar to the previous test case, but now the beleaguered fleet is in circular movement with the weaker attacker. So, the circular movement fails.
--
-- England: 
-- F North Sea - Denmark
-- F Yorkshire Supports F Norway - North Sea
--
-- Germany: 
-- F Holland Supports F Helgoland Bight - North Sea
-- F Helgoland Bight - North Sea
-- F Denmark - Helgoland Bight
--
-- Russia: 
-- F Skagerrak Supports F Norway - North Sea
-- F Norway - North Sea
--
-- There is no movement of fleets. 
--
-- Note: this reveals a bug in the resolver, as the Russian fleet in Norway
-- succeeds into the North Sea. Reason: when classifying this move, the resolver
-- does not find that the English fleet in the North Sea is a returning move, 
-- because without the Russian move into the North Sea, the German move into
-- the North Sea succeeds and the 3-cycle goes around unhindered.
-- We remove the Russian move when calssifying this so that we protect
-- ourselves from nontermination due to cycles in the move graph, but
-- apparently this is too drastic.
-- Solution: commit 1b92936. We compute the incumbant under the assumption
-- that the current move succeeds, rather than the assumption that the current
-- move was not given.
sixE10 = (expectedResolution == testTypicalResolution expectedResolution) ~? "6.E.10"
  where

    expectedResolution = M.fromList [
          (Zone (Normal NorthSea), (align Fleet England, SomeResolved (MoveObject (Normal Denmark), Just (MoveBounced (AtLeast (VCons (align (Fleet, Normal Denmark) Germany) VNil) [])))))
        , (Zone (Normal Yorkshire), (align Fleet England, SomeResolved (SupportObject (Fleet, Normal Norway) (Normal NorthSea), Nothing)))

        , (Zone (Normal Holland), (align Fleet Germany, SomeResolved (SupportObject (Fleet, Normal HeligolandBight) (Normal NorthSea), Nothing)))
        , (Zone (Normal HeligolandBight), (align Fleet Germany, SomeResolved (MoveObject (Normal NorthSea), Just (MoveOverpowered (AtLeast (VCons (align (Fleet, Normal Norway) Russia) VNil) [])))))
        , (Zone (Normal Denmark), (align Fleet Germany, SomeResolved (MoveObject (Normal HeligolandBight), Just (MoveBounced (AtLeast (VCons (align (Fleet, Normal HeligolandBight) Germany) VNil) [])))))

        , (Zone (Normal Skagerrak), (align Fleet Russia, SomeResolved (SupportObject (Fleet, Normal Norway) (Normal NorthSea), Nothing)))
        -- It bounces off the German fleet because in order to overpower it, it
        -- must have more non-English support than the German fleet, as the
        -- English fleet returns to North Sea!
        , (Zone (Normal Norway), (align Fleet Russia, SomeResolved (MoveObject (Normal NorthSea), Just (MoveBounced (AtLeast (VCons (align (Fleet, Normal HeligolandBight) Germany) VNil) [])))))

        ]

-- 6.E.11. TEST CASE, NO SELF DISLODGEMENT WITH BELEAGUERED GARRISON, UNIT SWAP WITH ADJACENT CONVOYING AND TWO COASTS
--
-- Similar to the previous test case, but now the beleaguered fleet is in a unit swap with the stronger attacker. So, the unit swap succeeds. To make the situation more complex, the swap is on an area with two coasts.
--
-- France: 
-- A Spain - Portugal via Convoy
-- F Mid-Atlantic Ocean Convoys A Spain - Portugal
-- F Gulf of Lyon Supports F Portugal - Spain(nc)
--
-- Germany: 
-- A Marseilles Supports A Gascony - Spain
-- A Gascony - Spain
--
-- Italy: 
-- F Portugal - Spain(nc)
-- F Western Mediterranean Supports F Portugal - Spain(nc)
--
-- The unit swap succeeds. Note that due to the success of the swap, there is no beleaguered garrison anymore. 
--
-- Note: this revealed a bug in the resolver. The Italian move would bounce off
-- of the German move because the French support was discounted. The French
-- move would fail because the Italian move is deemed to overpower it. I judge
-- this to be two independent bugs:
--   1. The French move: I was under the impression that this was supposed to
--      be overpowered, and that the Italian fleet ought to dislodge the
--      French move. Turns out that's only the case if the French army was not
--      convoyed.
--   2. The Italian move: the French support must be discounted ONLY IF the
--      Italian move would dislodge the French army, i.e. ONLY IF the French
--      army does not move to Portugal.
--
-- Solution implemented in 6951cbdb191361a1559fe4e50e6a565bd4665c05
--
sixE11 = (expectedResolution == testTypicalResolution expectedResolution) ~? "6.E.11"
  where

    expectedResolution = M.fromList [
          (Zone (Normal Spain), (align Army France, SomeResolved (MoveObject (Normal Portugal), Nothing)))
        , (Zone (Normal MidAtlanticOcean), (align Fleet France, SomeResolved (ConvoyObject (Army, Normal Spain) (Normal Portugal), Nothing)))
        , (Zone (Normal GulfOfLyon), (align Fleet France, SomeResolved (SupportObject (Fleet, Normal Portugal) (Special SpainNorth), Nothing)))

        , (Zone (Normal Marseilles), (align Army Germany, SomeResolved (SupportObject (Army, Normal Gascony) (Normal Spain), Nothing)))
        , (Zone (Normal Gascony), (align Army Germany, SomeResolved (MoveObject (Normal Spain), Just (MoveOverpowered (AtLeast (VCons (align (Fleet, Normal Portugal) Italy) VNil) [])))))

        , (Zone (Normal Portugal), (align Fleet Italy, SomeResolved (MoveObject (Special SpainNorth), Nothing)))
        , (Zone (Normal WesternMediterranean), (align Fleet Italy, SomeResolved (SupportObject (Fleet, Normal Portugal) (Special SpainNorth), Nothing)))
        ]

-- 6.E.12. TEST CASE, SUPPORT ON ATTACK ON OWN UNIT CAN BE USED FOR OTHER MEANS
--
-- A support on an attack on your own unit has still effect. It can prevent that another army will dislodge the unit.
--
-- Austria: 
-- A Budapest - Rumania
-- A Serbia Supports A Vienna - Budapest
--
-- Italy: 
-- A Vienna - Budapest
--
-- Russia: 
-- A Galicia - Budapest
-- A Rumania Supports A Galicia - Budapest
--
-- The support of Serbia on the Italian army prevents that the Russian army in Galicia will advance. No army will move. 
sixE12 = (expectedResolution == testTypicalResolution expectedResolution) ~? "6.E.12"
  where

    expectedResolution = M.fromList [
          (Zone (Normal Budapest), (align Army Austria, SomeResolved (MoveObject (Normal Rumania), Just (MoveBounced (AtLeast (VCons (align (Army, Normal Rumania) Russia) VNil) [])))))
        , (Zone (Normal Serbia), (align Army Austria, SomeResolved (SupportObject (Army, Normal Vienna) (Normal Budapest), Nothing)))

        -- The move from Vienna is overpowered by the Russian move, rather
        -- than bounced, because this contest does not include the Austrian
        -- support, for the Austrian army in Budapest is a returning move, and
        -- therefore Austrian support cannot be directed against it.
        , (Zone (Normal Vienna), (align Army Italy, SomeResolved (MoveObject (Normal Budapest), Just (MoveOverpowered (AtLeast (VCons (align (Army, Normal Galicia) Russia) VNil) [])))))

        , (Zone (Normal Galicia), (align Army Russia, SomeResolved (MoveObject (Normal Budapest), Just (MoveBounced (AtLeast (VCons (align (Army, Normal Vienna) Italy) VNil) [])))))
        , (Zone (Normal Rumania), (align Army Russia, SomeResolved (SupportObject (Army, Normal Galicia) (Normal Budapest), Nothing)))
        ]

-- 6.E.13. TEST CASE, THREE WAY BELEAGUERED GARRISON
--
-- In a beleaguered garrison from three sides, the adjudicator may not let two attacks fail and then let the third succeed.
--
-- England: 
-- F Edinburgh Supports F Yorkshire - North Sea
-- F Yorkshire - North Sea
--
-- France: 
-- F Belgium - North Sea
-- F English Channel Supports F Belgium - North Sea
--
-- Germany: 
-- F North Sea Hold
--
-- Russia: 
-- F Norwegian Sea - North Sea
-- F Norway Supports F Norwegian Sea - North Sea
--
-- None of the fleets move. The German fleet in the North Sea is not dislodged.
sixE13 = (expectedResolution == testTypicalResolution expectedResolution) ~? "6.E.13"
  where

    expectedResolution = M.fromList [
          (Zone (Normal Edinburgh), (align Fleet England, SomeResolved (SupportObject (Fleet, Normal Yorkshire) (Normal NorthSea), Nothing)))
        , (Zone (Normal Yorkshire), (align Fleet England, SomeResolved (MoveObject (Normal NorthSea), Just (MoveBounced (AtLeast (VCons (align (Fleet, Normal Belgium) France) VNil) [align (Fleet, Normal NorwegianSea) Russia])))))

        , (Zone (Normal Belgium), (align Fleet France, SomeResolved (MoveObject (Normal NorthSea), Just (MoveBounced (AtLeast (VCons (align (Fleet, Normal Yorkshire) England) VNil) [align (Fleet, Normal NorwegianSea) Russia])))))
        , (Zone (Normal EnglishChannel), (align Fleet France, SomeResolved (SupportObject (Fleet, Normal Belgium) (Normal NorthSea), Nothing)))

        , (Zone (Normal NorthSea), (align Fleet Germany, SomeResolved (MoveObject (Normal NorthSea), Nothing)))

        , (Zone (Normal NorwegianSea), (align Fleet Russia, SomeResolved (MoveObject (Normal NorthSea), Just (MoveBounced (AtLeast (VCons (align (Fleet, Normal Belgium) France) VNil) [align (Fleet, Normal Yorkshire) England])))))
        , (Zone (Normal Norway), (align Fleet Russia, SomeResolved (SupportObject (Fleet, Normal NorwegianSea) (Normal NorthSea), Nothing)))
        ]

-- 6.E.14. TEST CASE, ILLEGAL HEAD TO HEAD BATTLE CAN STILL DEFEND
--
-- If in a head to head battle, one of the units makes an illegal move, than that unit has still the possibility to defend against attacks with strength of one.
--
-- England: 
-- A Liverpool - Edinburgh
--
-- Russia: 
-- F Edinburgh - Liverpool
--
-- The move of the Russian fleet is illegal, but can still prevent the English army to enter Edinburgh. So, none of the units move.
--
-- Note: the Russian move is _invalid_, but the resolver doesn't know this. If
-- we pass it through the resolver, we ought to get MoveBounced. So here we
-- test the validation and the resolution.
sixE14 = (expectedResolution == testTypicalResolution expectedResolution && S.member MoveReachable validation) ~? "6.E.14"
  where

    validation = analyze snd (S.singleton . fst) S.empty S.union (moveVOC Russia occupation) russianMove
    occupation = occupy (Normal Edinburgh) (Just $ align Fleet Russia) emptyOccupation
    russianMove = Order ((Fleet, Normal Edinburgh), MoveObject (Normal Liverpool))

    expectedResolution = M.fromList [
          (Zone (Normal Liverpool), (align Army England, SomeResolved (MoveObject (Normal Edinburgh), Just (MoveBounced (AtLeast (VCons (align (Fleet, Normal Edinburgh) Russia) VNil) [])))))

        , (Zone (Normal Edinburgh), (align Fleet Russia, SomeResolved (MoveObject (Normal Liverpool), Just (MoveBounced (AtLeast (VCons (align (Army, Normal Liverpool) England) VNil) [])))))
        ]

-- The friendly head-to-head battle.
--
-- England:
--   F Holland Supports A Ruhr - Kiel
--   A Ruhr - Kiel
--
-- France:
--   A Kiel - Berlin
--   A Munich Supports A Kiel - Berlin
--   A Silesia Supports A Kiel - Berlin
--
-- Germany:
--   A Berlin - Kiel
--   F Denmark Supports A Berlin - Kiel
--   F Helgoland Bight Supports A Berlin - Kiel
--
-- Russia:
--   F Baltic Sea Supports A Prussia - Berlin
--   A Prussia - Berlin
--
-- No moves succeed.
sixE15 = (expectedResolution == resolution) ~? "6.E.15"
  where

    resolution = typicalResolution orders

    orders = M.fromList [
          (Zone (Normal Holland), (align Fleet England, SomeOrderObject (SupportObject (Army, Normal Ruhr) (Normal Kiel))))
        , (Zone (Normal Ruhr), (align Army England, SomeOrderObject (MoveObject (Normal Kiel))))

        , (Zone (Normal Kiel), (align Army France, SomeOrderObject (MoveObject (Normal Berlin))))
        , (Zone (Normal Munich), (align Army France, SomeOrderObject (SupportObject (Army, Normal Kiel) (Normal Berlin))))
        , (Zone (Normal Silesia), (align Army France, SomeOrderObject (SupportObject (Army, Normal Kiel) (Normal Berlin))))

        , (Zone (Normal Berlin), (align Army Germany, SomeOrderObject (MoveObject (Normal Kiel))))
        , (Zone (Normal Denmark), (align Fleet Germany, SomeOrderObject (SupportObject (Army, Normal Berlin) (Normal Kiel))))
        , (Zone (Normal HeligolandBight), (align Fleet Germany, SomeOrderObject (SupportObject (Army, Normal Berlin) (Normal Kiel))))

        , (Zone (Normal BalticSea), (align Fleet Russia, SomeOrderObject (SupportObject (Army, Normal Prussia) (Normal Berlin))))
        , (Zone (Normal Prussia), (align Army Russia, SomeOrderObject (MoveObject (Normal Berlin))))
        ]

    expectedResolution = M.fromList [
          (Zone (Normal Holland), (align Fleet England, SomeResolved (SupportObject (Army, Normal Ruhr) (Normal Kiel), Nothing)))
        , (Zone (Normal Ruhr), (align Army England, SomeResolved (MoveObject (Normal Kiel), Just (MoveOverpowered (AtLeast (VCons (align (Army, Normal Berlin) Germany) VNil) [])))))

        , (Zone (Normal Kiel), (align Army France, SomeResolved (MoveObject (Normal Berlin), Just (MoveBounced (AtLeast (VCons (align (Army, Normal Berlin) Germany) VNil) [])))))
        , (Zone (Normal Munich), (align Army France, SomeResolved (SupportObject (Army, Normal Kiel) (Normal Berlin), Nothing)))
        , (Zone (Normal Silesia), (align Army France, SomeResolved (SupportObject (Army, Normal Kiel) (Normal Berlin), Nothing)))

        , (Zone (Normal Berlin), (align Army Germany, SomeResolved (MoveObject (Normal Kiel), Just (MoveBounced (AtLeast (VCons (align (Army, Normal Kiel) France) VNil) [])))))
        , (Zone (Normal Denmark), (align Fleet Germany, SomeResolved (SupportObject (Army, Normal Berlin) (Normal Kiel), Nothing)))
        , (Zone (Normal HeligolandBight), (align Fleet Germany, SomeResolved (SupportObject (Army, Normal Berlin) (Normal Kiel), Nothing)))

        , (Zone (Normal BalticSea), (align Fleet Russia, SomeResolved (SupportObject (Army, Normal Prussia) (Normal Berlin), Nothing)))
        , (Zone (Normal Prussia), (align Army Russia, SomeResolved (MoveObject (Normal Berlin), Just (MoveOverpowered (AtLeast (VCons (align (Army, Normal Kiel) France) VNil) [])))))
        ]

-- 6.F.1. TEST CASE, NO CONVOY IN COASTAL AREAS
--
-- A fleet in a coastal area may not convoy.
--
-- Turkey: 
-- A Greece - Sevastopol
-- F Aegean Sea Convoys A Greece - Sevastopol
-- F Constantinople Convoys A Greece - Sevastopol
-- F Black Sea Convoys A Greece - Sevastopol
--
-- The convoy in Constantinople is not possible. So, the army in Greece will not move to Sevastopol. 
--
-- Note: our resolver is not concerned with validity of orders; it will let these
-- convoys go ahead. This test case is relevant to the validator.
sixF1 = S.member ConvoyValidSubject validation ~? "6.F.1"
  where

    validation = analyze snd (S.singleton . fst) S.empty S.union (convoyVOC Turkey occupation) convoyOrder
    convoyOrder = Order ((Fleet, Normal Constantinople), ConvoyObject (Army, Normal Greece) (Normal Sevastopol))
    occupation =
          occupy (Normal Constantinople) (Just $ align Fleet Turkey)
        . occupy (Normal Greece) (Just $ align Army Turkey)
        $ emptyOccupation

-- 6.F.2. TEST CASE, AN ARMY BEING CONVOYED CAN BOUNCE AS NORMAL
--
-- Armies being convoyed bounce on other units just as armies that are not being convoyed.
--
-- England: 
-- F English Channel Convoys A London - Brest
-- A London - Brest
--
-- France: 
-- A Paris - Brest
--
-- The English army in London bounces on the French army in Paris. Both units do not move. 
sixF2 = (expectedResolution == testTypicalResolution expectedResolution) ~? "6.F.2"
  where

    expectedResolution = M.fromList [
          (Zone (Normal EnglishChannel), (align Fleet England, SomeResolved (ConvoyObject (Army, Normal London) (Normal Brest), Nothing)))
        , (Zone (Normal London), (align Army England, SomeResolved (MoveObject (Normal Brest), Just (MoveBounced (AtLeast (VCons (align (Army, Normal Paris) France) VNil) [])))))

        , (Zone (Normal Paris), (align Army France, SomeResolved (MoveObject (Normal Brest), Just (MoveBounced (AtLeast (VCons (align (Army, Normal London) England) VNil) [])))))
        ]

-- 6.F.3. TEST CASE, AN ARMY BEING CONVOYED CAN RECEIVE SUPPORT
--
-- Armies being convoyed can receive support as in any other move.
--
-- England: 
-- F English Channel Convoys A London - Brest
-- A London - Brest
-- F Mid-Atlantic Ocean Supports A London - Brest
--
-- France: 
-- A Paris - Brest
--
-- The army in London receives support and beats the army in Paris. This means that the army London will end in Brest and the French army in Paris stays in Paris. 
sixF3 = (expectedResolution == testTypicalResolution expectedResolution) ~? "6.F.3"
  where

    expectedResolution = M.fromList [
          (Zone (Normal EnglishChannel), (align Fleet England, SomeResolved (ConvoyObject (Army, Normal London) (Normal Brest), Nothing)))
        , (Zone (Normal London), (align Army England, SomeResolved (MoveObject (Normal Brest), Nothing)))
        , (Zone (Normal MidAtlanticOcean), (align Fleet England, SomeResolved (SupportObject (Army, Normal London) (Normal Brest), Nothing)))

        , (Zone (Normal Paris), (align Army France, SomeResolved (MoveObject (Normal Brest), Just (MoveOverpowered (AtLeast (VCons (align (Army, Normal London) England) VNil) [])))))
        ]

-- 6.F.4. TEST CASE, AN ATTACKED CONVOY IS NOT DISRUPTED
--
-- A convoy can only be disrupted by dislodging the fleets. Attacking is not sufficient.
--
-- England: 
-- F North Sea Convoys A London - Holland
-- A London - Holland
--
-- Germany: 
-- F Skagerrak - North Sea
--
-- The army in London will successfully convoy and end in Holland. 
sixF4 = (expectedResolution == testTypicalResolution expectedResolution) ~? "6.F.4"
  where

    expectedResolution = M.fromList [
          (Zone (Normal NorthSea), (align Fleet England, SomeResolved (ConvoyObject (Army, Normal London) (Normal Holland), Nothing)))
        , (Zone (Normal London), (align Army England, SomeResolved (MoveObject (Normal Holland), Nothing)))

        , (Zone (Normal Skagerrak), (align Fleet Germany, SomeResolved (MoveObject (Normal NorthSea), Just (MoveBounced (AtLeast (VCons (align (Fleet, Normal NorthSea) England) VNil) [])))))
        ] 

-- 6.F.5. TEST CASE, A BELEAGUERED CONVOY IS NOT DISRUPTED
--
-- Even when a convoy is in a beleaguered garrison it is not disrupted.
--
-- England: 
-- F North Sea Convoys A London - Holland
-- A London - Holland
--
-- France: 
-- F English Channel - North Sea
-- F Belgium Supports F English Channel - North Sea
--
-- Germany: 
-- F Skagerrak - North Sea
-- F Denmark Supports F Skagerrak - North Sea
--
-- The army in London will successfully convoy and end in Holland. 
sixF5 = (expectedResolution == testTypicalResolution expectedResolution) ~? "6.F.5"
  where

    expectedResolution = M.fromList [
          (Zone (Normal NorthSea), (align Fleet England, SomeResolved (ConvoyObject (Army, Normal London) (Normal Holland), Nothing)))
        , (Zone (Normal London), (align Army England, SomeResolved (MoveObject (Normal Holland), Nothing)))

        , (Zone (Normal EnglishChannel), (align Fleet France, SomeResolved (MoveObject (Normal NorthSea), Just (MoveBounced (AtLeast (VCons (align (Fleet, Normal Skagerrak) Germany) VNil) [])))))
        , (Zone (Normal Belgium), (align Fleet France, SomeResolved (SupportObject (Fleet, Normal EnglishChannel) (Normal NorthSea), Nothing)))

        , (Zone (Normal Skagerrak), (align Fleet Germany, SomeResolved (MoveObject (Normal NorthSea), Just (MoveBounced (AtLeast (VCons (align (Fleet, Normal EnglishChannel) France) VNil) [])))))
        , (Zone (Normal Denmark), (align Fleet Germany, SomeResolved (SupportObject (Fleet, Normal Skagerrak) (Normal NorthSea), Nothing)))
        ]

-- 6.F.6. TEST CASE, DISLODGED CONVOY DOES NOT CUT SUPPORT
--
-- When a fleet of a convoy is dislodged, the convoy is completely cancelled. So, no support is cut.
--
-- England: 
-- F North Sea Convoys A London - Holland
-- A London - Holland
--
-- Germany: 
-- A Holland Supports A Belgium
-- A Belgium Supports A Holland
-- F Helgoland Bight Supports F Skagerrak - North Sea
-- F Skagerrak - North Sea
--
-- France: 
-- A Picardy - Belgium
-- A Burgundy Supports A Picardy - Belgium
--
-- The hold order of Holland on Belgium will sustain and Belgium will not be dislodged by the French in Picardy.
sixF6 = (expectedResolution == testTypicalResolution expectedResolution) ~? "6.F.6"
  where

    expectedResolution = M.fromList [
          (Zone (Normal NorthSea), (align Fleet England, SomeResolved (ConvoyObject (Army, Normal London) (Normal Holland), Just (ConvoyRouteCut [(Zone (Normal NorthSea), align (Fleet, Normal Skagerrak) Germany)]))))
        , (Zone (Normal London), (align Army England, SomeResolved (MoveObject (Normal Holland), Just MoveNoConvoy)))

        , (Zone (Normal Holland), (align Army Germany, SomeResolved (SupportObject (Army, Normal Belgium) (Normal Belgium), Nothing)))
        , (Zone (Normal Belgium), (align Army Germany, SomeResolved (SupportObject (Army, Normal Holland) (Normal Holland), Just (SupportCut (AtLeast (VCons (align (Army, Normal Picardy) France) VNil) [])))))
        , (Zone (Normal HeligolandBight), (align Fleet Germany, SomeResolved (SupportObject (Fleet, Normal Skagerrak) (Normal NorthSea), Nothing)))
        , (Zone (Normal Skagerrak), (align Fleet Germany, SomeResolved (MoveObject (Normal NorthSea), Nothing)))

        , (Zone (Normal Picardy), (align Army France, SomeResolved (MoveObject (Normal Belgium), Just (MoveBounced (AtLeast (VCons (align (Army, Normal Belgium) Germany) VNil) [])))))
        , (Zone (Normal Burgundy), (align Army France, SomeResolved (SupportObject (Army, Normal Picardy) (Normal Belgium), Nothing)))
        ]

-- 6.F.7. TEST CASE, DISLODGED CONVOY DOES NOT CAUSE CONTESTED AREA
--
-- When a fleet of a convoy is dislodged, the landing area is not contested, so other units can retreat to that area.
--
-- England: 
-- F North Sea Convoys A London - Holland
-- A London - Holland
--
-- Germany: 
-- F Helgoland Bight Supports F Skagerrak - North Sea
-- F Skagerrak - North Sea
--
-- The dislodged English fleet can retreat to Holland. 
--
-- Note: this tests withdraw validation and typical resolution.
sixF7 = (  S.null validation
        && expectedResolution == resolution) ~? "6.F.7"
  where

    validation = analyze snd (S.singleton . fst) S.empty S.union (withdrawVOC England resolution) withdrawOrder
    withdrawOrder = Order ((Fleet, Normal NorthSea), WithdrawObject (Normal Holland))
    resolution = testTypicalResolution expectedResolution
    expectedResolution = M.fromList [
          (Zone (Normal NorthSea), (align Fleet England, SomeResolved (ConvoyObject (Army, Normal London) (Normal Holland), Just (ConvoyRouteCut [(Zone (Normal NorthSea), align (Fleet, Normal Skagerrak) Germany)]))))
        , (Zone (Normal London), (align Army England, SomeResolved (MoveObject (Normal Holland), Just MoveNoConvoy)))

        , (Zone (Normal HeligolandBight), (align Fleet Germany, SomeResolved (SupportObject (Fleet, Normal Skagerrak) (Normal NorthSea), Nothing)))
        , (Zone (Normal Skagerrak), (align Fleet Germany, SomeResolved (MoveObject (Normal NorthSea), Nothing)))
        ]
    occupation = occupy (Normal NorthSea) (Just (align Fleet England)) emptyOccupation


    

-- 6.F.8. TEST CASE, DISLODGED CONVOY DOES NOT CAUSE A BOUNCE
--
-- When a fleet of a convoy is dislodged, then there will be no bounce in the landing area.
--
-- England: 
-- F North Sea Convoys A London - Holland
-- A London - Holland
--
-- Germany: 
-- F Helgoland Bight Supports F Skagerrak - North Sea
-- F Skagerrak - North Sea
-- A Belgium - Holland
--
-- The army in Belgium will not bounce and move to Holland. 
sixF8 = (expectedResolution == testTypicalResolution expectedResolution) ~? "6.F.8"
  where

    expectedResolution = M.fromList [
          (Zone (Normal NorthSea), (align Fleet England, SomeResolved (ConvoyObject (Army, Normal London) (Normal Holland), Just (ConvoyRouteCut [(Zone (Normal NorthSea), align (Fleet, Normal Skagerrak) Germany)]))))
        , (Zone (Normal London), (align Army England, SomeResolved (MoveObject (Normal Holland), Just MoveNoConvoy)))

        , (Zone (Normal HeligolandBight), (align Fleet Germany, SomeResolved (SupportObject (Fleet, Normal Skagerrak) (Normal NorthSea), Nothing)))
        , (Zone (Normal Skagerrak), (align Fleet Germany, SomeResolved (MoveObject (Normal NorthSea), Nothing)))
        , (Zone (Normal Belgium), (align Army Germany, SomeResolved (MoveObject (Normal Holland), Nothing)))
        ]

-- 6.F.9. TEST CASE, DISLODGE OF MULTI-ROUTE CONVOY
--
-- When a fleet of a convoy with multiple routes is dislodged, the result depends on the rulebook that is used.
--
-- England: 
-- F English Channel Convoys A London - Belgium
-- F North Sea Convoys A London - Belgium
-- A London - Belgium
--
-- France: 
-- F Brest Supports F Mid-Atlantic Ocean - English Channel
-- F Mid-Atlantic Ocean - English Channel
--
-- The French fleet in Mid Atlantic Ocean will dislodge the convoying fleet in the English Channel. If the 1971 rules are used (see issue 4.A.1), this will disrupt the convoy and the army will stay in London. When the 1982 or 2000 rulebook is used (which I prefer) the army can still go via the North Sea and the convoy succeeds and the London army will end in Belgium. 
--
-- Note: here we use the modern rules; the move will succeed.
sixF9 = (expectedResolution == testTypicalResolution expectedResolution) ~? "6.F.9"
  where

    expectedResolution = M.fromList [
          (Zone (Normal EnglishChannel), (align Fleet England, SomeResolved (ConvoyObject (Army, Normal London) (Normal Belgium), Just (ConvoyRouteCut [(Zone (Normal EnglishChannel), align (Fleet, Normal MidAtlanticOcean) France)]))))
        , (Zone (Normal NorthSea), (align Fleet England, SomeResolved (ConvoyObject (Army, Normal London) (Normal Belgium), Nothing)))
        , (Zone (Normal London), (align Army England, SomeResolved (MoveObject (Normal Belgium), Nothing)))

        , (Zone (Normal Brest), (align Fleet France, SomeResolved (SupportObject (Fleet, Normal MidAtlanticOcean) (Normal EnglishChannel), Nothing)))
        , (Zone (Normal MidAtlanticOcean), (align Fleet France, SomeResolved (MoveObject (Normal EnglishChannel), Nothing)))
        ]

-- 6.F.10. TEST CASE, DISLODGE OF MULTI-ROUTE CONVOY WITH FOREIGN FLEET
--
-- When the 1971 rulebook is used "unwanted" multi-route convoys are possible.
--
-- England: 
-- F North Sea Convoys A London - Belgium
-- A London - Belgium
--
-- Germany: 
-- F English Channel Convoys A London - Belgium
--
-- France: 
-- F Brest Supports F Mid-Atlantic Ocean - English Channel
-- F Mid-Atlantic Ocean - English Channel
--
-- If the 1982 or 2000 rulebook is used (which I prefer), it makes no difference that the convoying fleet in the English Channel is German. It will take the convoy via the North Sea anyway and the army in London will end in Belgium. However, when the 1971 rules are used, the German convoy is "unwanted". According to the DPTG the German fleet should be ignored in the English convoy, since there is a convoy path with only English fleets. That means that the convoy is not disrupted and the English army in London will end in Belgium. See also issue 4.A.1. 
--
-- Note: we use the modern rules; expectation is that the English army
-- successfully moves to Belgium.
sixF10 = (expectedResolution == testTypicalResolution expectedResolution) ~? "6.F.10"
  where

    expectedResolution = M.fromList [
          (Zone (Normal NorthSea), (align Fleet England, SomeResolved (ConvoyObject (Army, Normal London) (Normal Belgium), Nothing)))
        , (Zone (Normal London), (align Army England, SomeResolved (MoveObject (Normal Belgium), Nothing)))

        , (Zone (Normal EnglishChannel), (align Fleet Germany, SomeResolved (ConvoyObject (Army, Normal London) (Normal Belgium), Just (ConvoyRouteCut [(Zone (Normal EnglishChannel), align (Fleet, Normal MidAtlanticOcean) France)]))))

        , (Zone (Normal Brest), (align Fleet France, SomeResolved (SupportObject (Fleet, Normal MidAtlanticOcean) (Normal EnglishChannel), Nothing)))
        , (Zone (Normal MidAtlanticOcean), (align Fleet France, SomeResolved (MoveObject (Normal EnglishChannel), Nothing)))
        ]

-- 6.F.11. TEST CASE, DISLODGE OF MULTI-ROUTE CONVOY WITH ONLY FOREIGN FLEETS
--
-- When the 1971 rulebook is used, "unwanted" convoys can not be ignored in all cases.
--
-- England: 
-- A London - Belgium
--
-- Germany: 
-- F English Channel Convoys A London - Belgium
--
-- Russia: 
-- F North Sea Convoys A London - Belgium
--
-- France: 
-- F Brest Supports F Mid-Atlantic Ocean - English Channel
-- F Mid-Atlantic Ocean - English Channel
--
-- If the 1982 or 2000 rulebook is used (which I prefer), it makes no difference that the convoying fleets are not English. It will take the convoy via the North Sea anyway and the army in London will end in Belgium.
--
-- However, when the 1971 rules are used, the situation is different. Since both the fleet in the English Channel as the fleet in North Sea are not English, it can not be concluded that the German fleet is "unwanted". Therefore, one of the routes of the convoy is disrupted and that means that the complete convoy is disrupted. The army in London will stay in London. See also issue 4.A.1. 
--
-- Note: we use the modern rules; expectation is that the English army moves
-- to Belgium.
sixF11 = (expectedResolution == testTypicalResolution expectedResolution) ~? "6.F.11"
  where

    expectedResolution = M.fromList [
          (Zone (Normal London), (align Army England, SomeResolved (MoveObject (Normal Belgium), Nothing)))

        , (Zone (Normal EnglishChannel), (align Fleet Germany, SomeResolved (ConvoyObject (Army, Normal London) (Normal Belgium), Just (ConvoyRouteCut [(Zone (Normal EnglishChannel), align (Fleet, Normal MidAtlanticOcean) France)]))))

        , (Zone (Normal NorthSea), (align Fleet Russia, SomeResolved (ConvoyObject (Army, Normal London) (Normal Belgium), Nothing)))

        , (Zone (Normal Brest), (align Fleet France, SomeResolved (SupportObject (Fleet, Normal MidAtlanticOcean) (Normal EnglishChannel), Nothing)))
        , (Zone (Normal MidAtlanticOcean), (align Fleet France, SomeResolved (MoveObject (Normal EnglishChannel), Nothing)))
        ]

-- 6.F.12. TEST CASE, DISLODGED CONVOYING FLEET NOT ON ROUTE
--
-- When the rule is used that convoys are disrupted when one of the routes is disrupted (see issue 4.A.1), the convoy is not necessarily disrupted when one of the fleets ordered to convoy is dislodged.
--
-- England: 
-- F English Channel Convoys A London - Belgium
-- A London - Belgium
-- F Irish Sea Convoys A London - Belgium
--
-- France: 
-- F North Atlantic Ocean Supports F Mid-Atlantic Ocean - Irish Sea
-- F Mid-Atlantic Ocean - Irish Sea
--
-- Even when convoys are disrupted when one of the routes is disrupted (see issue 4.A.1), the convoy from London to Belgium will still succeed, since the dislodged fleet in the Irish Sea is not part of any route, although it can be reached from the starting point London. 
--
-- Note: I find this one difficult to parse. Here we just test that dislodging
-- that Irish Sea convoyer has no effect on the English move/convoy to Belgium.
-- The Irish Sea convoyer fails because there's no route from London to
-- Belgium which includes him.
sixF12 = (expectedResolution == testTypicalResolution expectedResolution) ~? "6.F.2"
  where

    expectedResolution = M.fromList [
          (Zone (Normal EnglishChannel), (align Fleet England, SomeResolved (ConvoyObject (Army, Normal London) (Normal Belgium), Nothing)))
        , (Zone (Normal London), (align Army England, SomeResolved (MoveObject (Normal Belgium), Nothing)))
        , (Zone (Normal IrishSea), (align Fleet England, SomeResolved (ConvoyObject (Army, Normal London) (Normal Belgium), Just ConvoyNoRoute)))

        , (Zone (Normal NorthAtlanticOcean), (align Fleet France, SomeResolved (SupportObject (Fleet, Normal MidAtlanticOcean) (Normal IrishSea), Nothing)))
        , (Zone (Normal MidAtlanticOcean), (align Fleet France, SomeResolved (MoveObject (Normal IrishSea), Nothing)))
      ]

-- 6.F.13. TEST CASE, THE UNWANTED ALTERNATIVE
--
-- This situation is not difficult to adjudicate, but it shows that even if someone wants to convoy, the player might not want an alternative route for the convoy.
--
-- England: 
-- A London - Belgium
-- F North Sea Convoys A London - Belgium
--
-- France: 
-- F English Channel Convoys A London - Belgium
--
-- Germany: 
-- F Holland Supports F Denmark - North Sea
-- F Denmark - North Sea
--
-- If France and German are allies, England want to keep its army in London, to defend the island. An army in Belgium could easily be destroyed by an alliance of France and Germany. England tries to be friends with Germany, however France and Germany trick England.
--
-- The convoy of the army in London succeeds and the fleet in Denmark dislodges the fleet in the North Sea. 
sixF13 = (expectedResolution == testTypicalResolution expectedResolution) ~? "6.F.13"
  where

    expectedResolution = M.fromList [
          (Zone (Normal London), (align Army England, SomeResolved (MoveObject (Normal Belgium), Nothing)))
        , (Zone (Normal NorthSea), (align Fleet England, SomeResolved (ConvoyObject (Army, Normal London) (Normal Belgium), Just (ConvoyRouteCut [(Zone (Normal NorthSea), align (Fleet, Normal Denmark) Germany)]))))

        , (Zone (Normal EnglishChannel), (align Fleet France, SomeResolved (ConvoyObject (Army, Normal London) (Normal Belgium), Nothing)))

        , (Zone (Normal Holland), (align Fleet Germany, SomeResolved (SupportObject (Fleet, Normal Denmark) (Normal NorthSea), Nothing)))
        , (Zone (Normal Denmark), (align Fleet Germany, SomeResolved (MoveObject (Normal NorthSea), Nothing)))
        ]

-- 6.F.14. TEST CASE, SIMPLE CONVOY PARADOX
--
-- The most common paradox is when the attacked unit supports an attack on one of the convoying fleets.
--
-- England: 
-- F London Supports F Wales - English Channel
-- F Wales - English Channel
--
-- France: 
-- A Brest - London
-- F English Channel Convoys A Brest - London
--
-- This situation depends on how paradoxes are handled (see issue (4.A.2). In case of the 'All Hold' rule (fully applied, not just as "backup" rule), both the movement of the English fleet in Wales as the France convoy in Brest are part of the paradox and fail. In all other rules of paradoxical convoys (including the Szykman rule which I prefer), the support of London is not cut. That means that the fleet in the English Channel is dislodged. 
--
-- Note: we choose to, in cases of convoy paradox, treat the convoying move (the
-- one which would cut the support against the convoy) as though it were a hold.
-- So, we expect the French fleet to be dislodged.
sixF14 = (expectedResolution == testTypicalResolution expectedResolution) ~? "6.F.14"
  where

    expectedResolution = M.fromList [
          (Zone (Normal London), (align Fleet England, SomeResolved (SupportObject (Fleet, Normal Wales) (Normal EnglishChannel), Nothing)))
        , (Zone (Normal Wales), (align Fleet England, SomeResolved (MoveObject (Normal EnglishChannel), Nothing)))

        , (Zone (Normal Brest), (align Army France, SomeResolved (MoveObject (Normal London), Just MoveConvoyParadox)))
        , (Zone (Normal EnglishChannel), (align Fleet France, SomeResolved (ConvoyObject (Army, Normal Brest) (Normal London), Just (ConvoyRouteCut [(Zone (Normal EnglishChannel), align (Fleet, Normal Wales) England)]))))
        ]

-- 6.F.15. TEST CASE, SIMPLE CONVOY PARADOX WITH ADDITIONAL CONVOY
--
-- Paradox rules only apply on the paradox core.
--
-- England: 
-- F London Supports F Wales - English Channel
-- F Wales - English Channel
--
-- France: 
-- A Brest - London
-- F English Channel Convoys A Brest - London
--
-- Italy: 
-- F Irish Sea Convoys A North Africa - Wales
-- F Mid-Atlantic Ocean Convoys A North Africa - Wales
-- A North Africa - Wales
--
-- The Italian convoy is not part of the paradox core and should therefore succeed when the move of the fleet in Wales is successful. This is the case except when the 'All Hold' paradox rule is used (fully applied, not just as "backup" rule, see issue 4.A.2).
--
-- I prefer the Szykman rule, so I prefer that both the fleet in Wales as the army in North Africa succeed in moving.
sixF15 = (expectedResolution == testTypicalResolution expectedResolution) ~? "6.F.15"
  where

    expectedResolution = M.fromList [
          (Zone (Normal London), (align Fleet England, SomeResolved (SupportObject (Fleet, Normal Wales) (Normal EnglishChannel), Nothing)))
        , (Zone (Normal Wales), (align Fleet England, SomeResolved (MoveObject (Normal EnglishChannel), Nothing)))

        , (Zone (Normal Brest), (align Army France, SomeResolved (MoveObject (Normal London), Just MoveConvoyParadox)))
        , (Zone (Normal EnglishChannel), (align Fleet France, SomeResolved (ConvoyObject (Army, Normal Brest) (Normal London), Just (ConvoyRouteCut [(Zone (Normal EnglishChannel), align (Fleet, Normal Wales) England)]))))

        , (Zone (Normal IrishSea), (align Fleet Italy, SomeResolved (ConvoyObject (Army, Normal NorthAfrica) (Normal Wales), Nothing)))
        , (Zone (Normal MidAtlanticOcean), (align Fleet Italy, SomeResolved (ConvoyObject (Army, Normal NorthAfrica) (Normal Wales), Nothing)))
        , (Zone (Normal NorthAfrica), (align Army Italy, SomeResolved (MoveObject (Normal Wales), Nothing)))
        ]

-- 6.F.16. TEST CASE, PANDIN'S PARADOX
--
-- In Pandin's paradox, the attacked unit protects the convoying fleet by a beleaguered garrison.
--
-- England: 
-- F London Supports F Wales - English Channel
-- F Wales - English Channel
--
-- France: 
-- A Brest - London
-- F English Channel Convoys A Brest - London
--
-- Germany: 
-- F North Sea Supports F Belgium - English Channel
-- F Belgium - English Channel
--
-- In all the different rules for resolving convoy disruption paradoxes (see issue 4.A.2), the support of London is not cut. That means that the fleet in the English Channel is not dislodged and none of the units succeed to move. 
sixF16 = (expectedResolution == testTypicalResolution expectedResolution) ~? "6.F.16"
  where

    expectedResolution = M.fromList [
          (Zone (Normal London), (align Fleet England, SomeResolved (SupportObject (Fleet, Normal Wales) (Normal EnglishChannel), Nothing)))
        , (Zone (Normal Wales), (align Fleet England, SomeResolved (MoveObject (Normal EnglishChannel), Just (MoveBounced (AtLeast (VCons (align (Fleet, Normal Belgium) Germany) VNil) [])))))

        , (Zone (Normal Brest), (align Army France, SomeResolved (MoveObject (Normal London), Just MoveConvoyParadox)))
        , (Zone (Normal EnglishChannel), (align Fleet France, SomeResolved (ConvoyObject (Army, Normal Brest) (Normal London), Nothing)))

        , (Zone (Normal NorthSea), (align Fleet Germany, SomeResolved (SupportObject (Fleet, Normal Belgium) (Normal EnglishChannel), Nothing)))
        , (Zone (Normal Belgium), (align Fleet Germany, SomeResolved (MoveObject (Normal EnglishChannel), Just (MoveBounced (AtLeast (VCons (align (Fleet, Normal Wales) England) VNil) [])))))
        ]

-- 6.F.17. TEST CASE, PANDIN'S EXTENDED PARADOX
--
-- In Pandin's extended paradox, the attacked unit protects the convoying fleet by a beleaguered garrison and the attacked unit can dislodge the unit that gives the protection.
--
-- England: 
-- F London Supports F Wales - English Channel
-- F Wales - English Channel
--
-- France: 
-- A Brest - London
-- F English Channel Convoys A Brest - London
-- F Yorkshire Supports A Brest - London
--
-- Germany: 
-- F North Sea Supports F Belgium - English Channel
-- F Belgium - English Channel
--
-- When the 1971, 1982 or 2000 rule is used (see issue 4.A.2), the support of London is not cut. That means that the fleet in the English Channel is not dislodged. The convoy will succeed and dislodge the fleet in London. You may argue that this violates the dislodge rule, but the common interpretation is that the paradox convoy rules take precedence over the dislodge rule.
--
-- If the Simon Szykman alternative is used (which I prefer), the convoy fails and the fleet in London and the English Channel are not dislodged. When the 'All Hold' (fully applied, not just as "backup" rule) or the DPTG rule is used, the result is the same as the Simon Szykman alternative. The involved moves (the move of the German fleet in Belgium and the convoying army in Brest) fail. 
--
-- Note: we also prefer the Simon Szykman alternative :)
sixF17 = (expectedResolution == testTypicalResolution expectedResolution) ~? "6.F.17"
  where

    expectedResolution = M.fromList [
          (Zone (Normal London), (align Fleet England, SomeResolved (SupportObject (Fleet, Normal Wales) (Normal EnglishChannel), Nothing)))
        , (Zone (Normal Wales), (align Fleet England, SomeResolved (MoveObject (Normal EnglishChannel), Just (MoveBounced (AtLeast (VCons (align (Fleet, Normal Belgium) Germany) VNil) [])))))

        , (Zone (Normal Brest), (align Army France, SomeResolved (MoveObject (Normal London), Just MoveConvoyParadox)))
        , (Zone (Normal EnglishChannel), (align Fleet France, SomeResolved (ConvoyObject (Army, Normal Brest) (Normal London), Nothing)))
        , (Zone (Normal Yorkshire), (align Fleet France, SomeResolved (SupportObject (Army, Normal Brest) (Normal London), Nothing)))

        , (Zone (Normal NorthSea), (align Fleet Germany, SomeResolved (SupportObject (Fleet, Normal Belgium) (Normal EnglishChannel), Nothing)))
        , (Zone (Normal Belgium), (align Fleet Germany, SomeResolved (MoveObject (Normal EnglishChannel), Just (MoveBounced (AtLeast (VCons (align (Fleet, Normal Wales) England) VNil) [])))))
        ]

-- 6.F.18. TEST CASE, BETRAYAL PARADOX
--
-- The betrayal paradox is comparable to Pandin's paradox, but now the attacked unit direct supports the convoying fleet. Of course, this will only happen when the player of the attacked unit is betrayed.
--
-- England: 
-- F North Sea Convoys A London - Belgium
-- A London - Belgium
-- F English Channel Supports A London - Belgium
--
-- France: 
-- F Belgium Supports F North Sea
--
-- Germany: 
-- F Helgoland Bight Supports F Skagerrak - North Sea
-- F Skagerrak - North Sea
--
-- If the English convoy from London to Belgium is successful, then it cuts the France support necessary to hold the fleet in the North Sea (see issue 4.A.2).
--
-- The 1971 and 2000 ruling do not give an answer on this.
--
-- According to the 1982 ruling the French support on the North Sea will not be cut. So, the fleet in the North Sea will not be dislodged by the Germans and the army in London will dislodge the French army in Belgium.
--
-- If the Szykman rule is followed (which I prefer), the move of the army in London will fail and will not cut support. That means that the fleet in the North Sea will not be dislodged.
--
-- The 'All Hold' rule has the same result as the Szykman rule, but with a different reason. The move of the army in London and the move of the German fleet in Skagerrak will fail.
--
-- Since a failing convoy does not result in a consistent resolution, the DPTG gives the same result as the 'All Hold' rule. 
--
-- Note: expected resolution follows the Szykman rule.
sixF18 = (expectedResolution == testTypicalResolution expectedResolution) ~? "6.F.18"
  where

    expectedResolution = M.fromList [
          (Zone (Normal NorthSea), (align Fleet England, SomeResolved (ConvoyObject (Army, Normal London) (Normal Belgium), Nothing)))
        , (Zone (Normal London), (align Army England, SomeResolved (MoveObject (Normal Belgium), Just MoveConvoyParadox)))
        , (Zone (Normal EnglishChannel), (align Fleet England, SomeResolved (SupportObject (Army, Normal London) (Normal Belgium), Nothing)))

        , (Zone (Normal Belgium), (align Fleet France, SomeResolved (SupportObject (Fleet, Normal NorthSea) (Normal NorthSea), Nothing)))

        , (Zone (Normal HeligolandBight), (align Fleet Germany, SomeResolved (SupportObject (Fleet, Normal Skagerrak) (Normal NorthSea), Nothing)))
        , (Zone (Normal Skagerrak), (align Fleet Germany, SomeResolved (MoveObject (Normal NorthSea), Just (MoveBounced (AtLeast (VCons (align (Fleet, Normal NorthSea) England) VNil) [])))))
        ]

-- 6.F.19. TEST CASE, MULTI-ROUTE CONVOY DISRUPTION PARADOX
--
-- The situation becomes more complex when the convoy has alternative routes.
--
-- France: 
-- A Tunis - Naples
-- F Tyrrhenian Sea Convoys A Tunis - Naples
-- F Ionian Sea Convoys A Tunis - Naples
--
-- Italy: 
-- F Naples Supports F Rome - Tyrrhenian Sea
-- F Rome - Tyrrhenian Sea
--
-- Now, two issues play a role. The ruling about disruption of convoys (issue 4.A.1) and the issue how paradoxes are resolved (issue 4.A.2).
--
-- If the 1971 rule is used about multi-route convoys (when one of the routes is disrupted, the convoy fails), this test case is just a simple paradox. For the 1971, 1982, 2000 and Szykman paradox rule, the support of the fleet in Naples is not cut and the fleet in Rome dislodges the fleet in the Tyrrhenian Sea. When the 'All Hold' rule is used, both the convoy of the army in Tunis as the move of the fleet in Rome will fail.
--
-- When the 1982 rule is used about multi-route convoy disruption, then convoys are disrupted when all routes are disrupted (this is the rule I prefer). With this rule, the situation becomes paradoxical. According to the 1971 and 1982 paradox rules, the support given by the fleet in Naples is not cut, that means that the fleet in the Tyrrhenian Sea is dislodged.
--
-- According to the 2000 ruling the fleet in the Tyrrhenian Sea is not "necessary" for the convoy and the support of Naples is cut and the fleet in the Tyrrhenian Sea is not dislodged.
--
-- If the Szykman rule is used (which I prefer), the 'All Hold' rule or the DPTG, then there is no paradoxical situation. The support of Naples is cut and the fleet in the Tyrrhenian Sea is not dislodged.
--
-- Note: as always, we go with Szykman.
sixF19 = (expectedResolution == testTypicalResolution expectedResolution) ~? "6.F.19"
  where

    expectedResolution = M.fromList [
          (Zone (Normal Tunis), (align Army France, SomeResolved (MoveObject (Normal Naples), Just (MoveBounced (AtLeast (VCons (align (Fleet, Normal Naples) Italy) VNil) [])))))
        , (Zone (Normal TyrrhenianSea), (align Fleet France, SomeResolved (ConvoyObject (Army, Normal Tunis) (Normal Naples), Nothing)))
        , (Zone (Normal IonianSea), (align Fleet France, SomeResolved (ConvoyObject (Army, Normal Tunis) (Normal Naples), Nothing)))

        , (Zone (Normal Naples), (align Fleet Italy, SomeResolved (SupportObject (Fleet, Normal Rome) (Normal TyrrhenianSea), Just (SupportCut (AtLeast (VCons (align (Army, Normal Tunis) France) VNil) [])))))
        , (Zone (Normal Rome), (align Fleet Italy, SomeResolved (MoveObject (Normal TyrrhenianSea), Just (MoveBounced (AtLeast (VCons (align (Fleet, Normal TyrrhenianSea) France) VNil) [])))))
        ]

-- 6.F.20. TEST CASE, UNWANTED MULTI-ROUTE CONVOY PARADOX
--
-- The 1982 paradox rule allows some creative defense.
--
-- France: 
-- A Tunis - Naples
-- F Tyrrhenian Sea Convoys A Tunis - Naples
--
-- Italy: 
-- F Naples Supports F Ionian Sea
-- F Ionian Sea Convoys A Tunis - Naples
--
-- Turkey: 
-- F Aegean Sea Supports F Eastern Mediterranean - Ionian Sea
-- F Eastern Mediterranean - Ionian Sea
--
-- Again, two issues play a role. The ruling about disruption of multi-route convoys (issue 4.A.1) and the issue how paradoxes are resolved (issue 4.A.2).
--
-- If the 1971 rule is used about multi-route convoys (when one of the routes is disrupted, the convoy fails), the Italian convoy order in the Ionian Sea is not part of the convoy, because it is a foreign unit (according to the DPTG). That means that the fleet in the Ionian Sea is not a 'convoying' fleet. In all rulings the support of Naples on the Ionian Sea is cut and the fleet in the Ionian Sea is dislodged by the Turkish fleet in the Eastern Mediterranean.
--
-- When the 1982 rule is used about multi-route convoy disruption, then convoys are disrupted when all routes are disrupted (this is the rule I prefer). With this rule, the situation becomes paradoxical. According to the 1971 and 1982 paradox rules, the support given by the fleet in Naples is not cut, that means that the fleet in the Ionian Sea is not dislodged.
--
-- According to the 2000 ruling the fleet in the Ionian Sea is not "necessary" and the support of Naples is cut and the fleet in the Ionian Sea is dislodged by the Turkish fleet in the Eastern Mediterranean.
--
-- If the Szykman rule, the 'All Hold' rule or DPTG is used, then there is no paradoxical situation. The support of Naples is cut and the fleet in the Ionian Sea is dislodged by the Turkish fleet in the Eastern Mediterranean.
--
-- As you can see, the 1982 rules allows the Italian player to save its fleet in the Ionian Sea with a trick. I do not consider this trick as normal tactical play. I prefer the Szykman rule as one of the rules that does not allow this trick. According to this rule the fleet in the Ionian Sea is dislodged.
--
-- Note: as always, we also prefer the Szykman rule.
sixF20 = (expectedResolution == testTypicalResolution expectedResolution) ~? "6.F.20"
  where

    expectedResolution = M.fromList [
          (Zone (Normal Tunis), (align Army France, SomeResolved (MoveObject (Normal Naples), Just (MoveBounced (AtLeast (VCons (align (Fleet, Normal Naples) Italy) VNil) [])))))
        , (Zone (Normal TyrrhenianSea), (align Fleet France, SomeResolved (ConvoyObject (Army, Normal Tunis) (Normal Naples), Nothing)))

        , (Zone (Normal Naples), (align Fleet Italy, SomeResolved (SupportObject (Fleet, Normal IonianSea) (Normal IonianSea), Just (SupportCut (AtLeast (VCons (align (Army, Normal Tunis) France) VNil) [])))))
        , (Zone (Normal IonianSea), (align Fleet Italy, SomeResolved (ConvoyObject (Army, Normal Tunis) (Normal Naples), Just (ConvoyRouteCut [(Zone (Normal IonianSea), align (Fleet, Normal EasternMediterranean) Turkey)]))))

        , (Zone (Normal AegeanSea), (align Fleet Turkey, SomeResolved (SupportObject (Fleet, Normal EasternMediterranean) (Normal IonianSea), Nothing)))
        , (Zone (Normal EasternMediterranean), (align Fleet Turkey, SomeResolved (MoveObject (Normal IonianSea), Nothing)))
        ]

-- 6.F.21. TEST CASE, DAD'S ARMY CONVOY
--
-- The 1982 paradox rule has as side effect that convoying armies do not cut support in some situations that are not paradoxical.
--
-- Russia: 
-- A Edinburgh Supports A Norway - Clyde
-- F Norwegian Sea Convoys A Norway - Clyde
-- A Norway - Clyde
--
-- France: 
-- F Irish Sea Supports F Mid-Atlantic Ocean - North Atlantic Ocean
-- F Mid-Atlantic Ocean - North Atlantic Ocean
--
-- England: 
-- A Liverpool - Clyde via Convoy
-- F North Atlantic Ocean Convoys A Liverpool - Clyde
-- F Clyde Supports F North Atlantic Ocean
--
-- In all rulings, except the 1982 paradox ruling, the support of the fleet in Clyde on the North Atlantic Ocean is cut and the French fleet in the Mid-Atlantic Ocean will dislodge the fleet in the North Atlantic Ocean. This is the preferred way.
--
-- However, in the 1982 paradox rule (see issue 4.A.2), the support of the fleet in Clyde is not cut. That means that the English fleet in the North Atlantic Ocean is not dislodged.
--
-- As you can see, the 1982 rule allows England to save its fleet in the North Atlantic Ocean in a very strange way. Just the support of Clyde is insufficient (if there is no convoy, the support is cut). Only the convoy to the area occupied by own unit, can do the trick in this situation. The embarking of troops in the fleet deceives the enemy so much that it works as a magic cloak. The enemy is not able to dislodge the fleet in the North Atlantic Ocean any more. Of course, this will only work in comedies. I prefer the Szykman rule as one of the rules that does not allow this trick. According to this rule (and all other paradox rules), the fleet in the North Atlantic is just dislodged.
--
-- Note: again, we too are sympathetic with the Szykman rule.
-- Slightly confused, though, as the Russian support seems unecessary. Oh well.
sixF21 = (expectedResolution == testTypicalResolution expectedResolution) ~? "6.F.21"
  where

    expectedResolution = M.fromList [
          (Zone (Normal Edinburgh), (align Army Russia, SomeResolved (SupportObject (Army, Normal Norway) (Normal Clyde), Nothing)))
        , (Zone (Normal NorwegianSea), (align Fleet Russia, SomeResolved (ConvoyObject (Army, Normal Norway) (Normal Clyde), Nothing)))
        , (Zone (Normal Norway), (align Army Russia, SomeResolved (MoveObject (Normal Clyde), Nothing)))

        , (Zone (Normal IrishSea), (align Fleet France, SomeResolved (SupportObject (Fleet, Normal MidAtlanticOcean) (Normal NorthAtlanticOcean), Nothing)))
        , (Zone (Normal MidAtlanticOcean), (align Fleet France, SomeResolved (MoveObject (Normal NorthAtlanticOcean), Nothing)))

        , (Zone (Normal Liverpool), (align Army England, SomeResolved (MoveObject (Normal Clyde), Just (MoveOverpowered (AtLeast (VCons (align (Army, Normal Norway) Russia) VNil) [])))))
        , (Zone (Normal NorthAtlanticOcean), (align Fleet England, SomeResolved (ConvoyObject (Army, Normal Liverpool) (Normal Clyde), Just (ConvoyRouteCut [(Zone (Normal NorthAtlanticOcean), align (Fleet, Normal MidAtlanticOcean) France)]))))
        , (Zone (Normal Clyde), (align Fleet England, SomeResolved (SupportObject (Fleet, Normal NorthAtlanticOcean) (Normal NorthAtlanticOcean), Just (SupportCut (AtLeast (VCons (align (Army, Normal Norway) Russia) VNil) [])))))
        ]

-- 6.F.22. TEST CASE, SECOND ORDER PARADOX WITH TWO RESOLUTIONS
--
-- Two convoys are involved in a second order paradox.
--
-- England: 
-- F Edinburgh - North Sea
-- F London Supports F Edinburgh - North Sea
--
-- France: 
-- A Brest - London
-- F English Channel Convoys A Brest - London
--
-- Germany: 
-- F Belgium Supports F Picardy - English Channel
-- F Picardy - English Channel
--
-- Russia: 
-- A Norway - Belgium
-- F North Sea Convoys A Norway - Belgium
--
-- Without any paradox rule, there are two consistent resolutions. The supports of the English fleet in London and the German fleet in Picardy are not cut. That means that the French fleet in the English Channel and the Russian fleet in the North Sea are dislodged, which makes it impossible to cut the support. The other resolution is that the supports of the English fleet in London the German fleet in Picardy are cut. In that case the French fleet in the English Channel and the Russian fleet in the North Sea will survive and will not be dislodged. This gives the possibility to cut the support.
--
-- The 1971 paradox rule and the 2000 rule (see issue 4.A.2) do not have an answer on this.
--
-- According to the 1982 rule, the supports are not cut which means that the French fleet in the English Channel and the Russian fleet in the North Sea are dislodged.
--
-- The Szykman (which I prefer), has the same result as the 1982 rule. The supports are not cut, the convoying armies fail to move, the fleet in Picardy dislodges the fleet in English Channel and the fleet in Edinburgh dislodges the fleet in the North Sea.
--
-- The DPTG rule has in this case the same result as the Szykman rule, because the failing of all convoys is a consistent resolution. So, the armies in Brest and Norway fail to move, while the fleets in Edinburgh and Picardy succeed to move.
--
-- When the 'All Hold' rule is used, the movement of the armies in Brest and Norway as the fleets in Edinburgh and Picardy will fail. 
--
-- Note: again, we prefer Szykman's rule.
-- This revealed a bug in the resolver, which looks so obvious in hindsight:
-- those convoyed moves (Brest, Norway) are not recognized as paradox-inducing!
-- Thus, the solver loops because, to know whether a support against a convoying
-- fleet is cut, it must determine whether that fleet can carry an army in order
-- to cut that support.
-- Solution: broaden the definition of a paradox-inducing convoy. At the time
-- of writing, a paradox-inducing convoy route is a route in which there is a
-- supporting unit at the target, whose support is directed against some zone
-- in that route. To accomodate n-order convoy paradoxes, we evidently must do
-- more searaching. 
-- The route must attack a support which is directed against some zone in a
-- route which attacks a support which is directed against some zone in a route
-- ... which attacks a support which is directed against some zone in that first
-- route.
sixF22 = (expectedResolution == testTypicalResolution expectedResolution) ~? "6.F.22"
  where

    expectedResolution = M.fromList [
          (Zone (Normal Edinburgh), (align Fleet England, SomeResolved (MoveObject (Normal NorthSea), Nothing)))
        , (Zone (Normal London), (align Fleet England, SomeResolved (SupportObject (Fleet, Normal Edinburgh) (Normal NorthSea), Nothing)))

        , (Zone (Normal Brest), (align Army France, SomeResolved (MoveObject (Normal London), Just MoveConvoyParadox)))
        , (Zone (Normal EnglishChannel), (align Fleet France, SomeResolved (ConvoyObject (Army, Normal Brest) (Normal London), Just (ConvoyRouteCut [(Zone (Normal EnglishChannel), align (Fleet, Normal Picardy) Germany)]))))

        , (Zone (Normal Belgium), (align Fleet Germany, SomeResolved (SupportObject (Fleet, Normal Picardy) (Normal EnglishChannel), Nothing)))
        , (Zone (Normal Picardy), (align Fleet Germany, SomeResolved (MoveObject (Normal EnglishChannel), Nothing)))

        , (Zone (Normal Norway), (align Army Russia, SomeResolved (MoveObject (Normal Belgium), Just MoveConvoyParadox)))
        , (Zone (Normal NorthSea), (align Fleet Russia, SomeResolved (ConvoyObject (Army, Normal Norway) (Normal Belgium), Just (ConvoyRouteCut [(Zone (Normal NorthSea), align (Fleet, Normal Edinburgh) England)]))))
        ]

-- 6.F.23. TEST CASE, SECOND ORDER PARADOX WITH TWO EXCLUSIVE CONVOYS
--
-- In this paradox there are two consistent resolutions, but where the two convoys do not fail or succeed at the same time. This fact is important for the DPTG resolution.
--
-- England: 
-- F Edinburgh - North Sea
-- F Yorkshire Supports F Edinburgh - North Sea
--
-- France: 
-- A Brest - London
-- F English Channel Convoys A Brest - London
--
-- Germany: 
-- F Belgium Supports F English Channel
-- F London Supports F North Sea
--
-- Italy: 
-- F Mid-Atlantic Ocean - English Channel
-- F Irish Sea Supports F Mid-Atlantic Ocean - English Channel
--
-- Russia: 
-- A Norway - Belgium
-- F North Sea Convoys A Norway - Belgium
--
-- Without any paradox rule, there are two consistent resolutions. In one resolution, the convoy in the English Channel is dislodged by the fleet in the Mid-Atlantic Ocean, while the convoy in the North Sea succeeds. In the other resolution, it is the other way around. The convoy in the North Sea is dislodged by the fleet in Edinburgh, while the convoy in the English Channel succeeds.
--
-- The 1971 paradox rule and the 2000 rule (see issue 4.A.2) do not have an answer on this.
--
-- According to the 1982 rule, the supports are not cut which means that the none of the units move.
--
-- The Szykman (which I prefer), has the same result as the 1982 rule. The convoying armies fail to move and the supports are not cut. Because of the failure to cut the support, no fleet succeeds to move.
--
-- When the 'All Hold' rule is used, the movement of the armies and the fleets all fail.
--
-- Since there is no consistent resolution where all convoys fail, the DPTG rule has the same result as the 'All Hold' rule. That means the movement of all units fail. 
--
-- Note: we prefer Szykman as well.
sixF23 = (expectedResolution == testTypicalResolution expectedResolution) ~? "6.F.23"
  where

    expectedResolution = M.fromList [
          (Zone (Normal Edinburgh), (align Fleet England, SomeResolved (MoveObject (Normal NorthSea), Just (MoveBounced (AtLeast (VCons (align (Fleet, Normal NorthSea) Russia) VNil) [])))))
        , (Zone (Normal Yorkshire), (align Fleet England, SomeResolved (SupportObject (Fleet, Normal Edinburgh) (Normal NorthSea), Nothing)))

        , (Zone (Normal Brest), (align Army France, SomeResolved (MoveObject (Normal London), Just MoveConvoyParadox)))
        , (Zone (Normal EnglishChannel), (align Fleet France, SomeResolved (ConvoyObject (Army, Normal Brest) (Normal London), Nothing)))

        , (Zone (Normal Belgium), (align Fleet Germany, SomeResolved (SupportObject (Fleet, Normal EnglishChannel) (Normal EnglishChannel), Nothing)))
        , (Zone (Normal London), (align Fleet Germany, SomeResolved (SupportObject (Fleet, Normal NorthSea) (Normal NorthSea), Nothing)))

        , (Zone (Normal MidAtlanticOcean), (align Fleet Italy, SomeResolved (MoveObject (Normal EnglishChannel), Just (MoveBounced (AtLeast (VCons (align (Fleet, Normal EnglishChannel) France) VNil) [])))))
        , (Zone (Normal IrishSea), (align Fleet Italy, SomeResolved (SupportObject (Fleet, Normal MidAtlanticOcean) (Normal EnglishChannel), Nothing)))

        , (Zone (Normal Norway), (align Army Russia, SomeResolved (MoveObject (Normal Belgium), Just MoveConvoyParadox)))
        , (Zone (Normal NorthSea), (align Fleet Russia, SomeResolved (ConvoyObject (Army, Normal Norway) (Normal Belgium), Nothing)))
        ]

-- 6.F.24. TEST CASE, SECOND ORDER PARADOX WITH NO RESOLUTION
--
-- As first order paradoxes, second order paradoxes come in two flavors, with two resolutions or no resolution.
--
-- England: 
-- F Edinburgh - North Sea
-- F London Supports F Edinburgh - North Sea
-- F Irish Sea - English Channel
-- F Mid-Atlantic Ocean Supports F Irish Sea - English Channel
--
-- France: 
-- A Brest - London
-- F English Channel Convoys A Brest - London
-- F Belgium Supports F English Channel
--
-- Russia: 
-- A Norway - Belgium
-- F North Sea Convoys A Norway - Belgium
--
-- When no paradox rule is used, there is no consistent resolution. If the French support in Belgium is cut, the French fleet in the English Channel will be dislodged. That means that the support of London will not be cut and the fleet in Edinburgh will dislodge the Russian fleet in the North Sea. In this way the support in Belgium is not cut! But if the support in Belgium is not cut, the Russian fleet in the North Sea will not be dislodged and the army in Norway can cut the support in Belgium.
--
-- The 1971 paradox rule and the 2000 rule (see issue 4.A.2) do not have an answer on this.
--
-- According to the 1982 rule, the supports are not cut which means that the French fleet in the English Channel will survive and but the Russian fleet in the North Sea is dislodged.
--
-- If the Szykman alternative is used (which I prefer), the supports are not cut and the convoying armies fail to move, which has the same result as the 1982 rule in this case.
--
-- When the 'All Hold' rule is used, the movement of the armies in Brest and Norway as the fleets in Edinburgh and the Irish Sea will fail.
--
-- Since there is no consistent resolution where all convoys fail, the DPTG has in this case the same result as the 'All Hold' rule.
--
-- Note: as always, we prefer Szykman as well.
sixF24 = (expectedResolution == testTypicalResolution expectedResolution) ~? "6.F.24"
  where

    expectedResolution = M.fromList [
          (Zone (Normal Edinburgh), (align Fleet England, SomeResolved (MoveObject (Normal NorthSea), Nothing)))
        , (Zone (Normal London), (align Fleet England, SomeResolved (SupportObject (Fleet, Normal Edinburgh) (Normal NorthSea), Nothing)))
        , (Zone (Normal IrishSea), (align Fleet England, SomeResolved (MoveObject (Normal EnglishChannel), Just (MoveBounced (AtLeast (VCons (align (Fleet, Normal EnglishChannel) France) VNil) [])))))
        , (Zone (Normal MidAtlanticOcean), (align Fleet England, SomeResolved (SupportObject (Fleet, Normal IrishSea) (Normal EnglishChannel), Nothing)))

        , (Zone (Normal Brest), (align Army France, SomeResolved (MoveObject (Normal London), Just MoveConvoyParadox)))
        , (Zone (Normal EnglishChannel), (align Fleet France, SomeResolved (ConvoyObject (Army, Normal Brest) (Normal London), Nothing)))
        , (Zone (Normal Belgium), (align Fleet France, SomeResolved (SupportObject (Fleet, Normal EnglishChannel) (Normal EnglishChannel), Nothing)))

        , (Zone (Normal Norway), (align Army Russia, SomeResolved (MoveObject (Normal Belgium), Just MoveConvoyParadox)))
        , (Zone (Normal NorthSea), (align Fleet Russia, SomeResolved (ConvoyObject (Army, Normal Norway) (Normal Belgium), Just (ConvoyRouteCut [(Zone (Normal NorthSea), align (Fleet, Normal Edinburgh) England)]))))
        ]

-- 6.H.1. TEST CASE, NO SUPPORTS DURING RETREAT
--
-- Supports are not allowed in the retreat phase.
--
-- Austria: 
-- F Trieste Hold
-- A Serbia Hold
--
-- Turkey: 
-- F Greece Hold
--
-- Italy: 
-- A Venice Supports A Tyrolia - Trieste
-- A Tyrolia - Trieste
-- F Ionian Sea - Greece
-- F Aegean Sea Supports F Ionian Sea - Greece
--
-- The fleet in Trieste and the fleet in Greece are dislodged. If the retreat orders are as follows:
--
-- Austria: 
-- F Trieste - Albania
-- A Serbia Supports F Trieste - Albania
--
-- Turkey: 
-- F Greece - Albania
--
-- The Austrian support order is illegal. Both dislodged fleets are disbanded. 
--
-- Note: this case is ruled out by the type system, so we give no test.
sixH1 = True ~? "6.H.1"

-- 6.H.2. TEST CASE, NO SUPPORTS FROM RETREATING UNIT
--
-- Even a retreating unit can not give support.
--
-- England: 
-- A Liverpool - Edinburgh
-- F Yorkshire Supports A Liverpool - Edinburgh
-- F Norway Hold
--
-- Germany: 
-- A Kiel Supports A Ruhr - Holland
-- A Ruhr - Holland
--
-- Russia: 
-- F Edinburgh Hold
-- A Sweden Supports A Finland - Norway
-- A Finland - Norway
-- F Holland Hold
--
-- The English fleet in Norway and the Russian fleets in Edinburgh and Holland are dislodged. If the following retreat orders are given:
--
-- England:
-- F Norway - North Sea
--
-- Russia:
-- F Edinburgh - North Sea
-- F Holland Supports F Edinburgh - North Sea
--
-- Although the fleet in Holland may receive an order, it may not support (it is disbanded). The English fleet in Norway and the Russian fleet in Edinburgh bounce and are disbanded. 
--
-- Note: this case is ruled out by the type system, so we give no test.
sixH2 = True ~? "6.H.2"

-- 6.H.3. TEST CASE, NO CONVOY DURING RETREAT
--
-- Convoys during retreat are not allowed.
--
-- England: 
-- F North Sea Hold
-- A Holland Hold
--
-- Germany: 
-- F Kiel Supports A Ruhr - Holland
-- A Ruhr - Holland
--
-- The English army in Holland is dislodged. If England orders the following in retreat:
--
-- England: 
-- A Holland - Yorkshire
-- F North Sea Convoys A Holland - Yorkshire
--
-- The convoy order is illegal. The army in Holland is disbanded.
--
-- Note: this case is ruled out by the type system, so we give no test.
sixH3 = True ~? "6.H.3"

-- 6.H.4. TEST CASE, NO OTHER MOVES DURING RETREAT
--
-- Of course you may not do any other move during a retreat. But look if the adjudicator checks for it.
--
-- England: 
-- F North Sea Hold
-- A Holland Hold
--
-- Germany: 
-- F Kiel Supports A Ruhr - Holland
-- A Ruhr - Holland
--
-- The English army in Holland is dislodged. If England orders the following in retreat:
--
-- England: 
-- A Holland - Belgium
-- F North Sea - Norwegian Sea
--
-- The fleet in the North Sea is not dislodge, so the move is illegal. 
--
-- Note: this case is ruled out by the type system, so we give no test
sixH4 = True ~? "6.H.4"

-- 6.H.5. TEST CASE, A UNIT MAY NOT RETREAT TO THE AREA FROM WHICH IT IS ATTACKED
--
-- Well, that would be of course stupid. Still, the adjudicator must be tested on this.
--
-- Russia: 
-- F Constantinople Supports F Black Sea - Ankara
-- F Black Sea - Ankara
--
-- Turkey: 
-- F Ankara Hold
--
-- Fleet in Ankara is dislodged and may not retreat to Black Sea. 
--
-- Note: this is principally a test of validation; we also have an implicit
-- test of resolution, though, as retreat phase validation depends upon
-- typical phase resolution.
sixH5 = (  S.member WithdrawNotDislodgingZone validation
        && expectedResolution == resolution) ~? "6.H.5"
  where

    validation = analyze snd (S.singleton . fst) S.empty S.union (withdrawVOC Turkey resolution) withdrawOrder
    withdrawOrder = Order ((Fleet, Normal Ankara), WithdrawObject (Normal BlackSea))
    resolution = testTypicalResolution expectedResolution
    expectedResolution = M.fromList [
          (Zone (Normal Constantinople), (align Fleet Russia, SomeResolved (SupportObject (Fleet, Normal BlackSea) (Normal Ankara), Nothing)))
        , (Zone (Normal BlackSea), (align Fleet Russia, SomeResolved (MoveObject (Normal Ankara), Nothing)))

        , (Zone (Normal Ankara), (align Fleet Turkey, SomeResolved (MoveObject (Normal Ankara), Just (MoveOverpowered (AtLeast (VCons (align (Fleet, Normal BlackSea) Russia) VNil) [])))))
        ]
    occupation = occupy (Normal Ankara) (Just (align Fleet Turkey)) emptyOccupation

-- 6.H.6. TEST CASE, UNIT MAY NOT RETREAT TO A CONTESTED AREA
--
-- Stand off prevents retreat to the area.
--
-- Austria: 
-- A Budapest Supports A Trieste - Vienna
-- A Trieste - Vienna
--
-- Germany: 
-- A Munich - Bohemia
-- A Silesia - Bohemia
--
-- Italy: 
-- A Vienna Hold
--
-- The Italian army in Vienna is dislodged. It may not retreat to Bohemia. 
--
-- Note: this tests validation and typical resolution.
sixH6 = (  S.member WithdrawUncontestedZone validation
        && expectedResolution == resolution) ~? "6.H.6"
  where

    validation = analyze snd (S.singleton . fst) S.empty S.union (withdrawVOC Italy resolution) withdrawOrder
    withdrawOrder = Order ((Army, Normal Vienna), WithdrawObject (Normal Bohemia))
    resolution = testTypicalResolution expectedResolution
    expectedResolution = M.fromList [
          (Zone (Normal Budapest), (align Army Austria, SomeResolved (SupportObject (Army, Normal Trieste) (Normal Vienna), Nothing)))
        , (Zone (Normal Trieste), (align Army Austria, SomeResolved (MoveObject (Normal Vienna), Nothing)))

        , (Zone (Normal Munich), (align Army Germany, SomeResolved (MoveObject (Normal Bohemia), Just (MoveBounced (AtLeast (VCons (align (Army, Normal Silesia) Germany) VNil) [])))))
        , (Zone (Normal Silesia), (align Army Germany, SomeResolved (MoveObject (Normal Bohemia), Just (MoveBounced (AtLeast (VCons (align (Army, Normal Munich) Germany) VNil) [])))))

        , (Zone (Normal Vienna), (align Army Italy, SomeResolved (MoveObject (Normal Vienna), Just (MoveOverpowered (AtLeast (VCons (align (Army, Normal Trieste) Austria) VNil) [])))))
        ]
    occupation = occupy (Normal Vienna) (Just (align Army Italy)) emptyOccupation

-- 6.H.7. TEST CASE, MULTIPLE RETREAT TO SAME AREA WILL DISBAND UNITS
--
-- There can only be one unit in an area.
--
-- Austria: 
-- A Budapest Supports A Trieste - Vienna
-- A Trieste - Vienna
--
-- Germany: 
-- A Munich Supports A Silesia - Bohemia
-- A Silesia - Bohemia
--
-- Italy: 
-- A Vienna Hold
-- A Bohemia Hold
--
-- If Italy orders the following for retreat:
--
-- Italy: 
-- A Bohemia - Tyrolia
-- A Vienna - Tyrolia
--
-- Both armies will be disbanded. 
sixH7 = (expectedResolution == testRetreatResolution expectedResolution) ~? "6.H.7"
  where

    expectedResolution = M.fromList [
          (Zone (Normal Bohemia), (align Army Italy, SomeResolved (WithdrawObject (Normal Tyrolia), Just (WithdrawCollision (AtLeast (VCons (align (Army, Normal Vienna) Italy) VNil) [])))))
        , (Zone (Normal Vienna), (align Army Italy, SomeResolved (WithdrawObject (Normal Tyrolia), Just (WithdrawCollision (AtLeast (VCons (align (Army, Normal Bohemia) Italy) VNil) [])))))
        ]

-- 6.H.8. TEST CASE, TRIPLE RETREAT TO SAME AREA WILL DISBAND UNITS
--
-- When three units retreat to the same area, then all three units are disbanded.
--
-- England: 
-- A Liverpool - Edinburgh
-- F Yorkshire Supports A Liverpool - Edinburgh
-- F Norway Hold
--
-- Germany: 
-- A Kiel Supports A Ruhr - Holland
-- A Ruhr - Holland
--
-- Russia: 
-- F Edinburgh Hold
-- A Sweden Supports A Finland - Norway
-- A Finland - Norway
-- F Holland Hold
--
-- The fleets in Norway, Edinburgh and Holland are dislodged. If the following retreat orders are given:
--
-- England: 
-- F Norway - North Sea
--
-- Russia: 
-- F Edinburgh - North Sea
-- F Holland - North Sea
--
-- All three units are disbanded. 
sixH8 = (expectedResolution == testRetreatResolution expectedResolution) ~? "6.H.8"
  where

    expectedResolution = M.fromList [
          (Zone (Normal Norway), (align Fleet England, SomeResolved (WithdrawObject (Normal NorthSea), Just (WithdrawCollision (AtLeast (VCons (align (Fleet, Normal Edinburgh) Russia) VNil) [align (Fleet, Normal Holland) Russia])))))

        , (Zone (Normal Edinburgh), (align Fleet Russia, SomeResolved (WithdrawObject (Normal NorthSea), Just (WithdrawCollision (AtLeast (VCons (align (Fleet, Normal Norway) England) VNil) [align (Fleet, Normal Holland) Russia])))))
        , (Zone (Normal Holland), (align Fleet Russia, SomeResolved (WithdrawObject (Normal NorthSea), Just (WithdrawCollision (AtLeast (VCons (align (Fleet, Normal Norway) England) VNil) [align (Fleet, Normal Edinburgh) Russia])))))
        ]

-- 6.H.9. TEST CASE, DISLODGED UNIT WILL NOT MAKE ATTACKERS AREA CONTESTED
--
-- An army can follow.
--
-- England: 
-- F Helgoland Bight - Kiel
-- F Denmark Supports F Helgoland Bight - Kiel
--
-- Germany: 
-- A Berlin - Prussia
-- F Kiel Hold
-- A Silesia Supports A Berlin - Prussia
--
-- Russia: 
-- A Prussia - Berlin
--
-- The fleet in Kiel can retreat to Berlin. 
sixH9 = (S.null validation && expectedResolution == resolution) ~? "6.H.9"
  where

    validation = analyze snd (S.singleton . fst) S.empty S.union (withdrawVOC Germany resolution) withdrawOrder
    withdrawOrder = Order ((Fleet, Normal Kiel), WithdrawObject (Normal Berlin))
    resolution = testTypicalResolution expectedResolution
    expectedResolution = M.fromList [
          (Zone (Normal HeligolandBight), (align Fleet England, SomeResolved (MoveObject (Normal Kiel), Nothing)))
        , (Zone (Normal Denmark), (align Fleet England, SomeResolved (SupportObject (Fleet, Normal HeligolandBight) (Normal Kiel), Nothing)))

        , (Zone (Normal Berlin), (align Army Germany, SomeResolved (MoveObject (Normal Prussia), Nothing)))
        , (Zone (Normal Kiel), (align Fleet Germany, SomeResolved (MoveObject (Normal Kiel), Just (MoveOverpowered (AtLeast (VCons (align (Fleet, Normal HeligolandBight) England) VNil) [])))))
        , (Zone (Normal Silesia), (align Army Germany, SomeResolved (SupportObject (Army, Normal Berlin) (Normal Prussia), Nothing)))

        , (Zone (Normal Prussia), (align Army Russia, SomeResolved (MoveObject (Normal Berlin), Just (MoveOverpowered (AtLeast (VCons (align (Army, Normal Berlin) Germany) VNil) [])))))
        ]
    occupation = occupy (Normal Kiel) (Just (align Fleet Germany)) emptyOccupation

-- 6.H.10. TEST CASE, NOT RETREATING TO ATTACKER DOES NOT MEAN CONTESTED
--
-- An army can not retreat to the place of the attacker. The easiest way to program that, is to mark that place as "contested". However, this is not correct. Another army may retreat to that place.
--
-- England: 
-- A Kiel Hold
--
-- Germany: 
-- A Berlin - Kiel
-- A Munich Supports A Berlin - Kiel
-- A Prussia Hold
--
-- Russia: 
-- A Warsaw - Prussia
-- A Silesia Supports A Warsaw - Prussia
--
-- The armies in Kiel and Prussia are dislodged. The English army in Kiel can not retreat to Berlin, but the army in Prussia can retreat to Berlin. Suppose the following retreat orders are given:
--
-- England:
-- A Kiel - Berlin
--
-- Germany:
-- A Prussia - Berlin
--
-- The English retreat to Berlin is illegal and fails (the unit is disbanded). The German retreat to Berlin is successful and does not bounce on the English unit. 
sixH10 = (  S.member WithdrawNotDislodgingZone englishValidation
         && S.null germanValidation
         && expectedResolution == resolution) ~? "6.H.10"
  where

    englishValidation = analyze snd (S.singleton . fst) S.empty S.union (withdrawVOC England resolution) englishWithdrawOrder
    germanValidation = analyze snd (S.singleton . fst) S.empty S.union (withdrawVOC Germany resolution) germanWithdrawOrder
    englishWithdrawOrder = Order ((Army, Normal Kiel), WithdrawObject (Normal Berlin))
    germanWithdrawOrder = Order ((Army, Normal Prussia), WithdrawObject (Normal Berlin))
    resolution = testTypicalResolution expectedResolution
    expectedResolution = M.fromList [
          (Zone (Normal Kiel), (align Army England, SomeResolved (MoveObject (Normal Kiel), Just (MoveOverpowered (AtLeast (VCons (align (Army, Normal Berlin) Germany) VNil) [])))))

        , (Zone (Normal Berlin), (align Army Germany, SomeResolved (MoveObject (Normal Kiel), Nothing)))
        , (Zone (Normal Munich), (align Army Germany, SomeResolved (SupportObject (Army, Normal Berlin) (Normal Kiel), Nothing)))
        , (Zone (Normal Prussia), (align Army Germany, SomeResolved (MoveObject (Normal Prussia), Just (MoveOverpowered (AtLeast (VCons (align (Army, Normal Warsaw) Russia) VNil) [])))))

        , (Zone (Normal Warsaw), (align Army Russia, SomeResolved (MoveObject (Normal Prussia), Nothing)))
        , (Zone (Normal Silesia), (align Army Russia, SomeResolved (SupportObject (Army, Normal Warsaw) (Normal Prussia), Nothing)))
        ]
    occupation =
          occupy (Normal Kiel) (Just (align Army England))
        . occupy (Normal Prussia) (Just (align Army Germany))
        $ emptyOccupation

-- 6.H.11. TEST CASE, RETREAT WHEN DISLODGED BY ADJACENT CONVOY
--
-- If a unit is dislodged by an army via convoy, the question arises whether the dislodged army can retreat to the original place of the convoyed army. This is only relevant in case the convoy was to an adjacent place.
--
-- France: 
-- A Gascony - Marseilles via Convoy
-- A Burgundy Supports A Gascony - Marseilles
-- F Mid-Atlantic Ocean Convoys A Gascony - Marseilles
-- F Western Mediterranean Convoys A Gascony - Marseilles
-- F Gulf of Lyon Convoys A Gascony - Marseilles
--
-- Italy: 
-- A Marseilles Hold
--
-- If for issue 4.A.3 choice b or c has been taken, then the army in Gascony will not move with the use of the convoy, because the army in Marseilles does not move in opposite direction. This immediately means that the army in Marseilles may not move to Gascony when it dislodged by the army there.
--
-- For all other choices of issue 4.A.3, the army in Gascony takes a convoy and does not pass the border of Gascony with Marseilles (it went a complete different direction). Now, the result depends on which rule is used for retreating (see issue 4.A.5).
--
-- I prefer the 1982/2000 rule for convoying to adjacent places. This means that the move of Gascony happened by convoy. Furthermore, I prefer that the army in Marseilles may retreat to Gascony.
--
-- Note: we go with the 1982/2000 rule.
sixH11 = (S.null validation && expectedResolution == resolution) ~? "6.H.11"
  where

    validation = analyze snd (S.singleton . fst) S.empty S.union (withdrawVOC Italy resolution) withdrawOrder
    withdrawOrder = Order ((Army, Normal Marseilles), WithdrawObject (Normal Gascony))
    resolution = testTypicalResolution expectedResolution
    expectedResolution = M.fromList [
          (Zone (Normal Gascony), (align Army France, SomeResolved (MoveObject (Normal Marseilles), Nothing)))
        , (Zone (Normal Burgundy), (align Army France, SomeResolved (SupportObject (Army, Normal Gascony) (Normal Marseilles), Nothing)))
        , (Zone (Normal MidAtlanticOcean), (align Fleet France, SomeResolved (ConvoyObject (Army, Normal Gascony) (Normal Marseilles), Nothing)))
        , (Zone (Normal WesternMediterranean), (align Fleet France, SomeResolved (ConvoyObject (Army, Normal Gascony) (Normal Marseilles), Nothing)))
        , (Zone (Normal GulfOfLyon), (align Fleet France, SomeResolved (ConvoyObject (Army, Normal Gascony) (Normal Marseilles), Nothing)))

        , (Zone (Normal Marseilles), (align Army Italy, SomeResolved (MoveObject (Normal Marseilles), Just (MoveOverpowered (AtLeast (VCons (align (Army, Normal Gascony) France) VNil) [])))))
        ]
    occupation = occupy (Normal Marseilles) (Just (align Army Italy)) emptyOccupation

-- 6.H.12. TEST CASE, RETREAT WHEN DISLODGED BY ADJACENT CONVOY WHILE TRYING TO DO THE SAME
--
-- The previous test case can be made more extra ordinary, when both armies tried to move by convoy.
--
-- England: 
-- A Liverpool - Edinburgh via Convoy
-- F Irish Sea Convoys A Liverpool - Edinburgh
-- F English Channel Convoys A Liverpool - Edinburgh
-- F North Sea Convoys A Liverpool - Edinburgh
--
-- France: 
-- F Brest - English Channel
-- F Mid-Atlantic Ocean Supports F Brest - English Channel
--
-- Russia: 
-- A Edinburgh - Liverpool via Convoy
-- F Norwegian Sea Convoys A Edinburgh - Liverpool
-- F North Atlantic Ocean Convoys A Edinburgh - Liverpool
-- A Clyde Supports A Edinburgh - Liverpool
--
-- If for issue 4.A.3 choice c has been taken, then the army in Liverpool will not try to move by convoy, because the convoy is disrupted. This has as consequence that army will just advance to Edinburgh by using the land route.
--
-- For all other choices of issue 4.A.3, both the army in Liverpool as in Edinburgh will try to move by convoy. The army in Edinburgh will succeed. The army in Liverpool will fail, because of the disrupted convoy. It is dislodged by the army of Edinburgh. Now, the question is whether the army in Liverpool may retreat to Edinburgh. The result depends on which rule is used for retreating (see issue 4.A.5).
--
-- I prefer the 1982/2000 rule for convoying to adjacent places. This means that the army in Liverpool tries the disrupted convoy. Furthermore, I prefer that the army in Liverpool may retreat to Edinburgh.
--
-- Note: we do not have the notion of explicit convoying! The English move to
-- Edinburgh will go over land, and because the Russian move is over water,
-- there is no conflict! The English and Russian moves succeed.
-- So, this test case is ignored radically altered, just to test the typical
-- resolution.
sixH12 = (expectedResolution == resolution) ~? "6.H.12"
  where

    resolution = testTypicalResolution expectedResolution
    expectedResolution = M.fromList [
          (Zone (Normal Liverpool), (align Army England, SomeResolved (MoveObject (Normal Edinburgh), Nothing)))
        , (Zone (Normal IrishSea), (align Fleet England, SomeResolved (ConvoyObject (Army, Normal Liverpool) (Normal Edinburgh), Just (ConvoyRouteCut [(Zone (Normal EnglishChannel), align (Fleet, Normal Brest) France)]))))
        , (Zone (Normal EnglishChannel), (align Fleet England, SomeResolved (ConvoyObject (Army, Normal Liverpool) (Normal Edinburgh), Just (ConvoyRouteCut [(Zone (Normal EnglishChannel), align (Fleet, Normal Brest) France)]))))
        , (Zone (Normal NorthSea), (align Fleet England, SomeResolved (ConvoyObject (Army, Normal Liverpool) (Normal Edinburgh), Just (ConvoyRouteCut [(Zone (Normal EnglishChannel), align (Fleet, Normal Brest) France)]))))

        , (Zone (Normal Brest), (align Fleet France, SomeResolved (MoveObject (Normal EnglishChannel), Nothing)))
        , (Zone (Normal MidAtlanticOcean), (align Fleet France, SomeResolved (SupportObject (Fleet, Normal Brest) (Normal EnglishChannel), Nothing)))

        , (Zone (Normal Edinburgh), (align Army Russia, SomeResolved (MoveObject (Normal Liverpool), Nothing)))
        , (Zone (Normal NorwegianSea), (align Fleet Russia, SomeResolved (ConvoyObject (Army, Normal Edinburgh) (Normal Liverpool), Nothing)))
        , (Zone (Normal NorthAtlanticOcean), (align Fleet Russia, SomeResolved (ConvoyObject (Army, Normal Edinburgh) (Normal Liverpool), Nothing)))
        , (Zone (Normal Clyde), (align Army Russia, SomeResolved (SupportObject (Army, Normal Edinburgh) (Normal Liverpool), Nothing)))
        ]

-- 6.H.13. TEST CASE, NO RETREAT WITH CONVOY IN MAIN PHASE
--
-- The places where a unit may retreat to, must be calculated during the main phase. Care should be taken that a convoy ordered in the main phase can not be used in the retreat phase.
--
-- England: 
-- A Picardy Hold
-- F English Channel Convoys A Picardy - London
--
-- France: 
-- A Paris - Picardy
-- A Brest Supports A Paris - Picardy
--
-- The dislodged army in Picardy can not retreat to London. 
--
-- Note: this is ruled out by the type system.
sixH13 = True ~? "6.H.13"

-- 6.H.14. TEST CASE, NO RETREAT WITH SUPPORT IN MAIN PHASE
--
-- Comparable to the previous test case, a support given in the main phase can not be used in the retreat phase.
--
-- England: 
-- A Picardy Hold
-- F English Channel Supports A Picardy - Belgium
--
-- France: 
-- A Paris - Picardy
-- A Brest Supports A Paris - Picardy
-- A Burgundy Hold
--
-- Germany: 
-- A Munich Supports A Marseilles - Burgundy
-- A Marseilles - Burgundy
--
-- After the main phase the following retreat orders are given: England: A Picardy - Belgium France: A Burgundy - Belgium
--
-- Both the army in Picardy and Burgundy are disbanded. 
--
-- Note: this is ruled out by the type system.
sixH14 = True ~? "6.H.14"

-- 6.H.15. TEST CASE, NO COASTAL CRAWL IN RETREAT
--
-- You can not go to the other coast from where the attacker came from.
--
-- England: 
-- F Portugal Hold
--
-- France: 
-- F Spain(sc) - Portugal
-- F Mid-Atlantic Ocean Supports F Spain(sc) - Portugal
--
-- The English fleet in Portugal is destroyed and can not retreat to Spain(nc). 
sixH15 = (  S.member WithdrawNotDislodgingZone validation
         && expectedResolution == resolution) ~? "6.H.15"
  where

    validation = analyze snd (S.singleton . fst) S.empty S.union (withdrawVOC England resolution) withdrawOrder
    withdrawOrder = Order ((Fleet, Normal Portugal), WithdrawObject (Special SpainNorth))
    resolution = testTypicalResolution expectedResolution
    expectedResolution = M.fromList [
          (Zone (Normal Portugal), (align Fleet England, SomeResolved (MoveObject (Normal Portugal), Just (MoveOverpowered (AtLeast (VCons (align (Fleet, Special SpainSouth) France) VNil) [])))))

        , (Zone (Special SpainSouth), (align Fleet France, SomeResolved (MoveObject (Normal Portugal), Nothing)))
        , (Zone (Normal MidAtlanticOcean), (align Fleet France, SomeResolved (SupportObject (Fleet, Special SpainSouth) (Normal Portugal), Nothing)))
        ]
    occupation = occupy (Normal Portugal) (Just (align Fleet England)) emptyOccupation

-- 6.H.16. TEST CASE, CONTESTED FOR BOTH COASTS
--
-- If a coast is contested, the other is not available for retreat.
--
-- France: 
-- F Mid-Atlantic Ocean - Spain(nc)
-- F Gascony - Spain(nc)
-- F Western Mediterranean Hold
--
-- Italy: 
-- F Tunis Supports F Tyrrhenian Sea - Western Mediterranean
-- F Tyrrhenian Sea - Western Mediterranean
--
-- The French fleet in the Western Mediterranean can not retreat to Spain(sc). 
sixH16 = (  S.member WithdrawUncontestedZone validation
         && expectedResolution == resolution) ~? "6.H.16"
  where

    validation = analyze snd (S.singleton . fst) S.empty S.union (withdrawVOC France resolution) withdrawOrder
    withdrawOrder = Order ((Fleet, Normal WesternMediterranean), WithdrawObject (Special SpainSouth))
    resolution = testTypicalResolution expectedResolution
    expectedResolution = M.fromList [
          (Zone (Normal MidAtlanticOcean), (align Fleet France, SomeResolved (MoveObject (Special SpainNorth), Just (MoveBounced (AtLeast (VCons (align (Fleet, Normal Gascony) France) VNil) [])))))
        , (Zone (Normal Gascony), (align Fleet France, SomeResolved (MoveObject (Special SpainNorth), Just (MoveBounced (AtLeast (VCons (align (Fleet, Normal MidAtlanticOcean) France) VNil) [])))))
        , (Zone (Normal WesternMediterranean), (align Fleet France, SomeResolved (MoveObject (Normal WesternMediterranean), Just (MoveOverpowered (AtLeast (VCons (align (Fleet, Normal TyrrhenianSea) Italy) VNil) [])))))

        , (Zone (Normal Tunis), (align Fleet Italy, SomeResolved (SupportObject (Fleet, Normal TyrrhenianSea) (Normal WesternMediterranean), Nothing)))
        , (Zone (Normal TyrrhenianSea), (align Fleet Italy, SomeResolved (MoveObject (Normal WesternMediterranean), Nothing)))
        ]
    occupation = occupy (Normal WesternMediterranean) (Just (align Fleet France)) emptyOccupation
