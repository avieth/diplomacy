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
    , sixC3
    , sixC4
    , sixC5
    , sixC6
    , sixC7
    , sixE15
    ]

isValid :: Either (InvalidReason phase order) t -> Bool
isValid = isRight

isInvalid :: Either (InvalidReason phase order) t -> Bool
isInvalid = isLeft

isMoveImpossible :: Maybe (InvalidReason Typical Move) -> Bool
isMoveImpossible = maybe False (== MoveImpossible)

isSupporterCouldNotDoMove :: Maybe (InvalidReason Typical Support) -> Bool
isSupporterCouldNotDoMove = maybe False (== SupporterCouldNotDoMove)

-- Moving to an area that is not a neighbour
--
-- England:
--   F North Sea - Picardy
sixA1 :: Test
sixA1 = isMoveImpossible validation ~? "6.A.1"
  where
    validation = validateMove occupation order
    occupation = occupy (Normal NorthSea) (Just $ align Fleet England) emptyOccupation
    order = Order (align ((Fleet, Normal NorthSea), MoveObject (Normal Picardy)) England)

-- Move army to sea
--
-- England:
--   A Liverpool - Irish Sea
sixA2 :: Test
sixA2 = isMoveImpossible validation ~? "6.A.2"
  where
    validation = validateMove occupation order
    occupation = occupy (Normal Liverpool) (Just $ align Army England) emptyOccupation
    order = Order (align ((Army, Normal Liverpool), MoveObject (Normal IrishSea)) England)

-- Move fleet to land
--
-- Germany:
--   F Kiel - Munich
sixA3 :: Test
sixA3 = isMoveImpossible validation ~? "6.A.3"
  where
    validation = validateMove occupation order
    occupation = occupy (Normal Kiel) (Just $ align Fleet Germany) emptyOccupation
    order = Order (align ((Fleet, Normal Kiel), MoveObject (Normal Munich)) Germany)

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
sixA9 = isMoveImpossible validation ~? "6.A.9"
  where
    validation = validateMove occupation order
    occupation = occupy (Normal Rome) (Just $ align Fleet Italy) emptyOccupation
    order = Order (align ((Fleet, Normal Rome), MoveObject (Normal Venice)) Italy)

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
sixA10 = isSupporterCouldNotDoMove validation ~? "6.A.10"
  where
    validation = validateSupport occupation order
    occupation = occupy (Normal Rome) (Just $ align Fleet Italy)
               . occupy (Normal Apulia) (Just $ align Army Italy)
               $ emptyOccupation
    order = Order (align ((Fleet, Normal Rome), SupportObject (Army, Normal Apulia) (Normal Venice)) Italy)

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
sixB4 = (isValid supportValidation && resolution == expectedResolution) ~? "6.B.4"
  where

    supportValidation = validate (validateSupport occupation) supportOrder

    supportOrder = Order (align ((Fleet, Normal Marseilles), SupportObject (Fleet, Normal Gascony) (Special SpainNorth)) France)

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
sixB5 = isSupporterCouldNotDoMove validation ~? "6.B.5"
  where

    validation = validateSupport occupation support
  
    support =
        Order (align ((Fleet, Special SpainNorth), SupportObject (Fleet, Normal Marseilles) (Normal GulfOfLyon)) France)

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
changeCoasts = isMoveImpossible validation ~? "changeCoasts"
  where
    validation = validateMove (occupy (Special SpainNorth) (Just (align Fleet France)) emptyOccupation) (Order (align ((Fleet, Special SpainNorth), MoveObject (Special SpainSouth)) France))

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
        -- Note the potentially confusing failure reason: the subject is
        -- a Turkish army in Ankara... could mean this was bounced by a move
        -- from Ankara to Smyrna, but in fact it means bounced by a failed move
        -- FROM Smyrna to Ankara.
        , (Zone (Normal Constantinople), (align Army Turkey, SomeResolved (MoveObject (Normal Smyrna), Just (MoveBounced (AtLeast (VCons (align (Army, Normal Ankara) Turkey) VNil) [])))))
        , (Zone (Normal Smyrna), (align Army Turkey, SomeResolved (MoveObject (Normal Ankara), Just (MoveBounced (AtLeast (VCons (align (Fleet, Normal Constantinople) Turkey) VNil) [])))))
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
          (Zone (Normal Trieste), (align Army Austria, SomeResolved (MoveObject (Normal Serbia), Just (MoveBounced (AtLeast (VCons (align (Army, Normal Bulgaria) Austria) VNil) [])))))
        , (Zone (Normal Serbia), (align Army Austria, SomeResolved (MoveObject (Normal Bulgaria), Just (MoveBounced (AtLeast (VCons (align (Army, Normal Trieste) Turkey) VNil) [])))))

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
        -- Warning: potentially confusing failure reason... it means the English
        -- army which was trying to move to Belgium causes this one to fail.
        , (Zone (Normal Belgium), (align Army France, SomeResolved (MoveObject (Normal London), Just (MoveBounced (AtLeast (VCons (align (Army, Normal Belgium) England) VNil) [])))))
        , (Zone (Normal Burgundy), (align Army France, SomeResolved (MoveObject (Normal Belgium), Just (MoveBounced (AtLeast (VCons (align (Army, Normal London) England) VNil) [])))))
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
        , (Zone (Normal HelgolandBright), (align Fleet Germany, SomeOrderObject (SupportObject (Army, Normal Berlin) (Normal Kiel))))

        , (Zone (Normal BalticSea), (align Fleet Russia, SomeOrderObject (SupportObject (Army, Normal Prussia) (Normal Berlin))))
        , (Zone (Normal Prussia), (align Army Russia, SomeOrderObject (MoveObject (Normal Berlin))))
        ]

    expectedResolution = M.fromList [
          (Zone (Normal Holland), (align Fleet England, SomeResolved (SupportObject (Army, Normal Ruhr) (Normal Kiel), Nothing)))
        , (Zone (Normal Ruhr), (align Army England, SomeResolved (MoveObject (Normal Kiel), Just (MoveOverpowered (AtLeast (VCons (align (Army, Normal Berlin) Germany) VNil) [])))))

        , (Zone (Normal Kiel), (align Army France, SomeResolved (MoveObject (Normal Berlin), Just (Move2Cycle (align Army Germany)))))
        , (Zone (Normal Munich), (align Army France, SomeResolved (SupportObject (Army, Normal Kiel) (Normal Berlin), Nothing)))
        , (Zone (Normal Silesia), (align Army France, SomeResolved (SupportObject (Army, Normal Kiel) (Normal Berlin), Nothing)))

        , (Zone (Normal Berlin), (align Army Germany, SomeResolved (MoveObject (Normal Kiel), Just (Move2Cycle (align Army France)))))
        , (Zone (Normal Denmark), (align Fleet Germany, SomeResolved (SupportObject (Army, Normal Berlin) (Normal Kiel), Nothing)))
        , (Zone (Normal HelgolandBright), (align Fleet Germany, SomeResolved (SupportObject (Army, Normal Berlin) (Normal Kiel), Nothing)))

        , (Zone (Normal BalticSea), (align Fleet Russia, SomeResolved (SupportObject (Army, Normal Prussia) (Normal Berlin), Nothing)))
        , (Zone (Normal Prussia), (align Army Russia, SomeResolved (MoveObject (Normal Berlin), Just (MoveOverpowered (AtLeast (VCons (align (Army, Normal Kiel) France) VNil) [])))))
        ]


