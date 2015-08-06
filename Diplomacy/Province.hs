{-|
Module      : Diplomacy.Province
Description : Definition of Province, ProvinceTarget.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Diplomacy.Province (

    Province(..)
  , allProvinces

  , adjacency
  , adjacent
  , isSameOrAdjacent

  , neighbours
  , isSameOrNeighbour
  , provinceCommonNeighbours
  , provinceCommonCoasts
  , commonNeighbours
  , commonCoasts
  , provinceWaterReachables
  , waterReachables

  , ProvinceType(..)
  , provinceType
  , supplyCentre
  , supplyCentres

  , isCoastal
  , isInland
  , isWater

  , country
  , isHome

  -- Verification
  , symmetryCheck
  , antireflexivityCheck

  , ProvinceTarget(..)
  , ProvinceCoast(..)

  , isNormal
  , isSpecial

  , pcProvince 
  , provinceCoasts
  , ptProvince

  , provinceTargets
  , properProvinceTargets
  , provinceTargetCluster
  , allProvinceTargets

  , shortestPath
  , distance
  , distanceFromHomeSupplyCentre

  , parseProvince
  , parseProvinceTarget

  , printProvince
  , printProvinceTarget

  , paths

  ) where

import Control.Monad (guard)
import Control.Applicative
import qualified Data.Set as S
import Data.String (fromString, IsString)
import Data.List (sort)
import Diplomacy.GreatPower
import Text.Parsec hiding ((<|>))
import Text.Parsec.Text

-- | Exhaustive enumeration of the places on the diplomacy board.
--   Refernce: https://www.wizards.com/avalonhill/rules/diplomacy.pdf
data Province
  = Bohemia
  | Budapest
  | Galicia
  | Trieste
  | Tyrolia
  | Vienna
  | Clyde
  | Edinburgh
  | Liverpool
  | London
  | Wales
  | Yorkshire
  | Brest
  | Burgundy
  | Gascony
  | Marseilles
  | Paris
  | Picardy
  | Berlin
  | Kiel
  | Munich
  | Prussia
  | Ruhr
  | Silesia
  | Apulia
  | Naples
  | Piedmont
  | Rome
  | Tuscany
  | Venice
  | Livonia
  | Moscow
  | Sevastopol
  | StPetersburg
  | Ukraine
  | Warsaw
  | Ankara
  | Armenia
  | Constantinople
  | Smyrna
  | Syria
  | Albania
  | Belgium
  | Bulgaria
  | Finland
  | Greece
  | Holland
  | Norway
  | NorthAfrica
  | Portugal
  | Rumania
  | Serbia
  | Spain
  | Sweden
  | Tunis
  | Denmark
  | AdriaticSea
  | AegeanSea
  | BalticSea
  | BarentsSea
  | BlackSea
  | EasternMediterranean
  | EnglishChannel
  | GulfOfBothnia
  | GulfOfLyon
  | HeligolandBight
  | IonianSea
  | IrishSea
  | MidAtlanticOcean
  | NorthAtlanticOcean
  | NorthSea
  | NorwegianSea
  | Skagerrak
  | TyrrhenianSea
  | WesternMediterranean
    deriving (Eq, Ord, Enum, Bounded, Show)

-- | The type of a province is important as it determines which units
--   can occupy it.
data ProvinceType = Inland | Water | Coastal
  deriving (Eq, Ord, Enum, Bounded, Show)

provinceType :: Province -> ProvinceType
provinceType Bohemia = Inland
provinceType Budapest = Inland
provinceType Galicia = Inland
provinceType Trieste = Coastal
provinceType Tyrolia = Inland
provinceType Vienna = Inland
provinceType Clyde = Coastal
provinceType Edinburgh = Coastal
provinceType Liverpool = Coastal
provinceType London = Coastal
provinceType Wales = Coastal
provinceType Yorkshire = Coastal
provinceType Brest = Coastal
provinceType Burgundy = Inland
provinceType Gascony = Coastal
provinceType Marseilles = Coastal
provinceType Paris = Inland
provinceType Picardy = Coastal
provinceType Berlin = Coastal
provinceType Kiel = Coastal
provinceType Munich = Inland
provinceType Prussia = Coastal
provinceType Ruhr = Inland
provinceType Silesia = Inland
provinceType Apulia = Coastal
provinceType Naples = Coastal
provinceType Piedmont = Coastal
provinceType Rome = Coastal
provinceType Tuscany = Coastal
provinceType Venice = Coastal
provinceType Livonia = Coastal
provinceType Moscow = Inland
provinceType Sevastopol = Coastal
provinceType StPetersburg = Coastal
provinceType Ukraine = Inland
provinceType Warsaw = Inland
provinceType Ankara = Coastal
provinceType Armenia = Coastal
provinceType Constantinople = Coastal
provinceType Smyrna = Coastal
provinceType Syria = Coastal
provinceType Albania = Coastal
provinceType Belgium = Coastal
provinceType Bulgaria = Coastal
provinceType Finland = Coastal
provinceType Greece = Coastal
provinceType Holland = Coastal
provinceType Norway = Coastal
provinceType NorthAfrica = Coastal
provinceType Portugal = Coastal
provinceType Rumania = Coastal
provinceType Serbia = Inland
provinceType Spain = Coastal
provinceType Sweden = Coastal
provinceType Tunis = Coastal
provinceType Denmark = Coastal
provinceType AdriaticSea = Water
provinceType AegeanSea = Water
provinceType BalticSea = Water
provinceType BarentsSea = Water
provinceType BlackSea = Water
provinceType EasternMediterranean = Water
provinceType EnglishChannel = Water
provinceType GulfOfBothnia = Water
provinceType GulfOfLyon = Water
provinceType HeligolandBight = Water
provinceType IonianSea = Water
provinceType IrishSea = Water
provinceType MidAtlanticOcean = Water
provinceType NorthAtlanticOcean = Water
provinceType NorthSea = Water
provinceType NorwegianSea = Water
provinceType Skagerrak = Water
provinceType TyrrhenianSea = Water
provinceType WesternMediterranean = Water

-- | This function describes adjacency between provinces.
--   TODO verify; I'm confused about the adjacency surrounding
--   Denmark.
adjacency :: Province -> [Province]
adjacency Bohemia = [Munich, Tyrolia, Vienna, Silesia, Galicia]
adjacency Budapest = [Vienna, Galicia, Rumania, Serbia, Trieste]
adjacency Galicia = [Warsaw, Silesia, Ukraine, Rumania, Budapest, Vienna, Bohemia]
adjacency Trieste = [AdriaticSea, Venice, Tyrolia, Vienna, Budapest, Serbia, Albania]
adjacency Tyrolia = [Piedmont, Munich, Bohemia, Vienna, Trieste, Venice]
adjacency Vienna = [Trieste, Tyrolia, Bohemia, Galicia, Budapest]
adjacency Clyde = [NorthAtlanticOcean, NorwegianSea, Edinburgh, Liverpool]
adjacency Edinburgh = [Clyde, NorwegianSea, NorthSea, Yorkshire, Liverpool]
adjacency Liverpool = [NorthAtlanticOcean, IrishSea, Clyde, Edinburgh, Yorkshire, Wales]
adjacency London = [NorthSea, EnglishChannel, Wales, Yorkshire]
adjacency Wales = [IrishSea, EnglishChannel, London, Yorkshire, Liverpool]
adjacency Yorkshire = [Liverpool, Edinburgh, London, Wales, NorthSea]
adjacency Brest = [EnglishChannel, MidAtlanticOcean, Picardy, Paris, Gascony]
adjacency Burgundy = [Paris, Picardy, Belgium, Ruhr, Munich, Marseilles, Gascony]
adjacency Gascony = [MidAtlanticOcean, Spain, Brest, Paris, Burgundy, Marseilles]
adjacency Marseilles = [GulfOfLyon, Spain, Gascony, Burgundy, Piedmont]
adjacency Paris = [Brest, Picardy, Burgundy, Gascony]
adjacency Picardy = [EnglishChannel, Belgium, Burgundy, Paris, Brest]
adjacency Berlin = [BalticSea, Prussia, Silesia, Munich, Kiel]
-- TODO verify this one!
adjacency Kiel = [HeligolandBight, Berlin, Munich, Ruhr, Holland, Denmark, BalticSea]
adjacency Munich = [Ruhr, Kiel, Berlin, Silesia, Bohemia, Tyrolia, Burgundy]
adjacency Prussia = [BalticSea, Livonia, Warsaw, Silesia, Berlin]
adjacency Ruhr = [Belgium, Holland, Kiel, Munich, Burgundy]
adjacency Silesia = [Munich, Berlin, Prussia, Warsaw, Galicia, Bohemia]
adjacency Apulia = [AdriaticSea, IonianSea, Naples, Rome, Venice]
adjacency Naples = [IonianSea, TyrrhenianSea, Apulia, Rome]
adjacency Piedmont = [Marseilles, Tyrolia, GulfOfLyon, Venice, Tuscany]
adjacency Rome = [TyrrhenianSea, Naples, Tuscany, Venice, Apulia]
adjacency Tuscany = [GulfOfLyon, Piedmont, Venice, Rome, TyrrhenianSea]
adjacency Venice = [Piedmont, Tyrolia, Trieste, AdriaticSea, Apulia, Tuscany, Rome]
adjacency Livonia = [BalticSea, GulfOfBothnia, StPetersburg, Moscow, Warsaw, Prussia]
adjacency Moscow = [StPetersburg, Sevastopol, Ukraine, Warsaw, Livonia]
adjacency Sevastopol = [Armenia, BlackSea, Rumania, Ukraine, Moscow]
adjacency StPetersburg = [BarentsSea, Moscow, Livonia, GulfOfBothnia, Finland, Norway]
adjacency Ukraine = [Moscow, Sevastopol, Rumania, Galicia, Warsaw]
adjacency Warsaw = [Prussia, Livonia, Moscow, Ukraine, Galicia, Silesia]
adjacency Ankara = [BlackSea, Armenia, Smyrna, Constantinople]
adjacency Armenia = [BlackSea, Sevastopol, Syria, Ankara, Smyrna]
adjacency Constantinople = [BlackSea, Ankara, Smyrna, Bulgaria, AegeanSea]
adjacency Smyrna = [EasternMediterranean, AegeanSea, Constantinople, Ankara, Armenia, Syria]
adjacency Syria = [Armenia, Smyrna, EasternMediterranean]
adjacency Albania = [AdriaticSea, Trieste, Serbia, Greece, IonianSea]
adjacency Belgium = [Holland, Ruhr, Burgundy, Picardy, EnglishChannel, NorthSea]
adjacency Bulgaria = [Rumania, BlackSea, Constantinople, AegeanSea, Greece, Serbia]
adjacency Finland = [StPetersburg, Sweden, Norway, GulfOfBothnia]
adjacency Greece = [IonianSea, AegeanSea, Albania, Serbia, Bulgaria]
adjacency Holland = [Belgium, NorthSea, Kiel, Ruhr, HeligolandBight]
adjacency Norway = [NorwegianSea, NorthSea, Sweden, Finland, Skagerrak, BarentsSea, StPetersburg]
adjacency NorthAfrica = [MidAtlanticOcean, WesternMediterranean, Tunis]
adjacency Portugal = [MidAtlanticOcean, Spain]
adjacency Rumania = [BlackSea, Bulgaria, Serbia, Budapest, Galicia, Ukraine, Sevastopol]
adjacency Serbia = [Trieste, Budapest, Rumania, Bulgaria, Greece, Albania]
adjacency Spain = [Portugal, MidAtlanticOcean, Gascony, GulfOfLyon, WesternMediterranean, Marseilles]
adjacency Sweden = [GulfOfBothnia, Finland, Norway, BalticSea, Skagerrak, Denmark]
adjacency Tunis = [NorthAfrica, WesternMediterranean, IonianSea, TyrrhenianSea]
adjacency Denmark = [BalticSea, Skagerrak, HeligolandBight, Kiel, NorthSea, Sweden]
adjacency AdriaticSea = [Trieste, Venice, Apulia, Albania, IonianSea]
adjacency AegeanSea = [Greece, Bulgaria, Constantinople, Smyrna, EasternMediterranean, IonianSea]
adjacency BalticSea = [Sweden, GulfOfBothnia, Livonia, Prussia, Berlin, Kiel, Denmark]
adjacency BarentsSea = [StPetersburg, Norway, NorwegianSea]
adjacency BlackSea = [Sevastopol, Armenia, Ankara, Constantinople, Bulgaria, Rumania]
adjacency EasternMediterranean = [Syria, IonianSea, AegeanSea, Smyrna]
adjacency EnglishChannel = [London, Belgium, Picardy, Brest, MidAtlanticOcean, IrishSea, Wales, NorthSea]
adjacency GulfOfBothnia = [Sweden, Finland, Livonia, StPetersburg, BalticSea]
adjacency GulfOfLyon = [Marseilles, Piedmont, Tuscany, TyrrhenianSea, WesternMediterranean, Spain]
adjacency HeligolandBight = [Denmark, Kiel, Holland, NorthSea]
adjacency IonianSea = [Tunis, TyrrhenianSea, Naples, Apulia, AdriaticSea, Greece, Albania, AegeanSea, EasternMediterranean]
adjacency IrishSea = [NorthAtlanticOcean, EnglishChannel, MidAtlanticOcean, Liverpool, Wales]
adjacency MidAtlanticOcean = [NorthAtlanticOcean, IrishSea, EnglishChannel, Brest, Gascony, Spain, Portugal, WesternMediterranean, NorthAfrica]
adjacency NorthAtlanticOcean = [NorwegianSea, Clyde, Liverpool, IrishSea, MidAtlanticOcean]
adjacency NorthSea = [NorwegianSea, Skagerrak, Denmark, HeligolandBight, Holland, Belgium, EnglishChannel, London, Yorkshire, Edinburgh, Norway]
adjacency NorwegianSea = [NorthAtlanticOcean, Norway, BarentsSea, NorthSea, Clyde, Edinburgh]
adjacency Skagerrak = [Norway, Sweden, Denmark, NorthSea]
adjacency TyrrhenianSea = [GulfOfLyon, WesternMediterranean, Tunis, Tuscany, Rome, Naples, IonianSea]
adjacency WesternMediterranean = [NorthAfrica, MidAtlanticOcean, GulfOfLyon, Spain, Tunis, TyrrhenianSea]

adjacent :: Province -> Province -> Bool
adjacent prv0 prv1 = prv0 `elem` (adjacency prv1)

isSameOrAdjacent :: Province -> Province -> Bool
isSameOrAdjacent prv0 prv1 = prv0 == prv1 || adjacent prv0 prv1

allProvinces :: [Province]
allProvinces = [minBound .. maxBound]

-- | A list of all pairs for which adjacency is not symmetric.
--   Should be []
symmetryCheck :: [(Province, Province)]
symmetryCheck = filter (not . adjacencyIsSymmetric) (pairs allProvinces)

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs l@(x:xs) = (map ((,) x) l) ++ (pairs xs)

adjacencyIsSymmetric :: (Province, Province) -> Bool
adjacencyIsSymmetric (p1, p2) =
  if adjacent p1 p2
  then adjacent p2 p1
  else not $ adjacent p2 p1

-- | A list of all pairs for which adjacency is reflexive (we want it to be
--   antireflexive).
antireflexivityCheck :: [Province]
antireflexivityCheck = filter (not . adjacencyIsAntireflexive) allProvinces

adjacencyIsAntireflexive :: Province -> Bool
adjacencyIsAntireflexive p = not $ p `adjacent` p

supplyCentre :: Province -> Bool
supplyCentre Norway = True
supplyCentre Sweden = True
supplyCentre Denmark = True
supplyCentre StPetersburg = True
supplyCentre Moscow = True
supplyCentre Sevastopol = True
supplyCentre Ankara = True
supplyCentre Smyrna = True
supplyCentre Constantinople = True
supplyCentre Rumania = True
supplyCentre Bulgaria = True
supplyCentre Greece = True
supplyCentre Serbia = True
supplyCentre Warsaw = True
supplyCentre Budapest = True
supplyCentre Vienna = True
supplyCentre Trieste = True
supplyCentre Berlin = True
supplyCentre Kiel = True
supplyCentre Munich = True
supplyCentre Venice = True
supplyCentre Rome = True
supplyCentre Naples = True
supplyCentre Tunis = True
supplyCentre Spain = True
supplyCentre Portugal = True
supplyCentre Marseilles = True
supplyCentre Paris = True
supplyCentre Brest = True
supplyCentre Belgium = True
supplyCentre Holland = True
supplyCentre London = True
supplyCentre Liverpool = True
supplyCentre Edinburgh = True
supplyCentre _ = False

supplyCentres :: [Province]
supplyCentres = filter supplyCentre [minBound..maxBound]

-- | Some provinces belong to a country.
--   This is useful in conjunction with supplyCentre to determine which
--   provinces can be used by a given country to build a unit.
country :: Province -> Maybe GreatPower
country Bohemia = Just Austria
country Budapest = Just Austria
country Galicia = Just Austria
country Trieste = Just Austria
country Tyrolia = Just Austria
country Vienna = Just Austria
country Clyde = Just England
country Edinburgh = Just England
country Liverpool = Just England
country London = Just England
country Wales = Just England
country Yorkshire = Just England
country Brest = Just France
country Burgundy = Just France
country Gascony = Just France
country Marseilles = Just France
country Paris = Just France
country Picardy = Just France
country Berlin = Just Germany
country Kiel = Just Germany
country Munich = Just Germany
country Prussia = Just Germany
country Ruhr = Just Germany
country Silesia = Just Germany
country Apulia = Just Italy
country Naples = Just Italy
country Piedmont = Just Italy
country Rome = Just Italy
country Tuscany = Just Italy
country Venice = Just Italy
country Livonia = Just Russia
country Moscow = Just Russia
country Sevastopol = Just Russia
country StPetersburg = Just Russia
country Ukraine = Just Russia
country Warsaw = Just Russia
country Ankara = Just Turkey
country Armenia = Just Turkey
country Constantinople = Just Turkey
country Smyrna = Just Turkey
country Syria = Just Turkey
country Albania = Nothing
country Belgium = Nothing
country Bulgaria = Nothing
country Finland = Nothing
country Greece = Nothing
country Holland = Nothing
country Norway = Nothing
country NorthAfrica = Nothing
country Portugal = Nothing
country Rumania = Nothing
country Serbia = Nothing
country Spain = Nothing
country Sweden = Nothing
country Tunis = Nothing
country Denmark = Nothing
country AdriaticSea = Nothing
country AegeanSea = Nothing
country BalticSea = Nothing
country BarentsSea = Nothing
country BlackSea = Nothing
country EasternMediterranean = Nothing
country EnglishChannel = Nothing
country GulfOfBothnia = Nothing
country GulfOfLyon = Nothing
country HeligolandBight = Nothing
country IonianSea = Nothing
country IrishSea = Nothing
country MidAtlanticOcean = Nothing
country NorthAtlanticOcean = Nothing
country NorthSea = Nothing
country NorwegianSea = Nothing
country Skagerrak = Nothing
country TyrrhenianSea = Nothing
country WesternMediterranean = Nothing

isHome :: GreatPower -> Province -> Bool
isHome c p = maybe False ((==) c) (country p)

-- | Province does not express all move order targets, like the north coast
--   of StPetersburg.
data ProvinceTarget
  = Normal Province
  -- TBD rename Special to MultiCoast or something else more descriptive?
  | Special ProvinceCoast
    deriving (Eq, Ord)

-- TODO should define a parser for ProvinceTarget as well, in such a way
-- that show pt always parses to pt.
instance Show ProvinceTarget where
  show (Normal province) = show province
  show (Special provinceCoast) = show provinceCoast

isSpecial :: ProvinceTarget -> Bool
isSpecial (Special _) = True
isSpecial _ = False

isNormal :: ProvinceTarget -> Bool
isNormal (Normal _) = True
isNormal _ = False

allProvinceTargets :: [ProvinceTarget]
allProvinceTargets = map Normal allProvinces ++ map Special (allProvinces >>= provinceCoasts)

-- | Like allProvinceTargets but those ProvinceTargets which have ProvinceCoasts
--   associated are eliminated from the list.
properProvinceTargets :: [ProvinceTarget]
properProvinceTargets = do
  Normal p <- map Normal allProvinces
  let cs = provinceCoasts p
  if null cs then [Normal p] else map Special cs

provinceTargets :: Province -> [ProvinceTarget]
provinceTargets x = Normal x : (map Special (provinceCoasts x))

provinceTargetCluster :: ProvinceTarget -> [ProvinceTarget]
provinceTargetCluster (Normal x) = provinceTargets x
provinceTargetCluster (Special c) = (Normal $ pcProvince c) : (map Special (provinceCoasts (pcProvince c)))

data ProvinceCoast
  = StPetersburgNorth
  | StPetersburgSouth
  | SpainNorth
  | SpainSouth
  | BulgariaEast
  | BulgariaSouth
    deriving (Eq, Ord, Enum, Bounded)

pcProvince :: ProvinceCoast -> Province
pcProvince StPetersburgNorth = StPetersburg
pcProvince StPetersburgSouth = StPetersburg
pcProvince SpainNorth = Spain
pcProvince SpainSouth = Spain
pcProvince BulgariaEast = Bulgaria
pcProvince BulgariaSouth = Bulgaria

provinceCoasts :: Province -> [ProvinceCoast]
provinceCoasts StPetersburg = [StPetersburgNorth, StPetersburgSouth]
provinceCoasts Spain = [SpainNorth, SpainSouth]
provinceCoasts Bulgaria = [BulgariaEast, BulgariaSouth]
provinceCoasts _ = []

ptProvince :: ProvinceTarget -> Province
ptProvince (Normal p) = p
ptProvince (Special c) = pcProvince c

isCoastal :: Province -> Bool
isCoastal prv = case provinceType prv of
  Coastal -> True
  _ -> False

isInland :: Province -> Bool
isInland prv = case provinceType prv of
  Inland -> True
  _ -> False

isWater :: Province -> Bool
isWater prv = case provinceType prv of
  Water -> True
  _ -> False

-- | True iff the given province should not be considered adjacent to the
--   given province coast, even though they are adjacent as provinces.
blacklist :: Province -> ProvinceTarget -> Bool
blacklist p (Special c) = coastBlacklist p c
  where
    coastBlacklist :: Province -> ProvinceCoast -> Bool
    coastBlacklist WesternMediterranean SpainNorth = True
    coastBlacklist GulfOfLyon SpainNorth = True
    coastBlacklist Gascony SpainSouth = True
    coastBlacklist Marseilles SpainNorth = True
    -- NB MidAtlanticOcean to SpainSouth is fine!
    coastBlacklist GulfOfBothnia StPetersburgNorth = True
    coastBlacklist BarentsSea StPetersburgSouth = True
    coastBlacklist BlackSea BulgariaSouth = True
    coastBlacklist AegeanSea BulgariaEast = True
    coastBlacklist _ _ = False
blacklist _ _ = False

-- TODO TODAY!!!
-- Define LISTS of valid moves and convoys from a given ProvinceTarget, then
-- redefine the above few methods in terms of this and `elem`.
-- commonCoast, as well.
-- Yes here we have definitions for Provinces which should be usable to
-- implement anoalogues for ProvinceTargets.

provinceCommonNeighbours :: Province -> Province -> [Province]
provinceCommonNeighbours province1 province2 =
    [ x | x <- adjacency province1, y <- adjacency province2, x == y ]

-- Fact: always [] if either argument is inland.
provinceCommonCoasts :: Province -> Province -> [Province]
provinceCommonCoasts province1 province2 =
    filter isWater (provinceCommonNeighbours province1 province2)

-- | List containing all Provinces reachable by a path through 1 or more
--   water territories. This can and often does include the input province.
--   Fact: if input is inland, output is empty.
--   Fact: if input is not inland, output contains no inland.
provinceWaterReachables :: Province -> [Province]
provinceWaterReachables province = provinceWaterReachables' waterNeighbours waterNeighbours

  where

    waterNeighbours = filter isWater (adjacency province)

    provinceWaterReachables' seenSoFar vanguard =
        let nextVanguard = [ y | x <- vanguard, y <- adjacency x, isWater y, not (elem y seenSoFar) ]
        in case nextVanguard of
             -- In this case seenSoFar consists only of water provinces. Grab
             -- all of their non-water (coastal) neighbours too.
             [] -> [ y | x <- seenSoFar, y <- adjacency x, not (isWater y) ] ++ seenSoFar
             _ -> provinceWaterReachables' (nextVanguard ++ seenSoFar) nextVanguard

-- | Exactly the same as provinceWaterReachables but we use neighbours instead
--   of adjacency.
waterReachables :: ProvinceTarget -> [ProvinceTarget]
waterReachables pt = waterReachables' waterNeighbours waterNeighbours

  where

    waterNeighbours = filter (isWater . ptProvince) (neighbours pt)

    waterReachables' seenSoFar vanguard =
        let nextVanguard = [ y | x <- vanguard, y <- neighbours x, isWater (ptProvince y), not (elem y seenSoFar) ]
        in case nextVanguard of
             [] -> [ y | x <- seenSoFar, y <- neighbours x, not (isWater (ptProvince y)) ] ++ seenSoFar
             _ -> waterReachables' (nextVanguard ++ seenSoFar) nextVanguard

-- | ProvinceTarget neighbours; this is like adjacency but for ProvinceTargets,
--   using the blacklist to handle the special coasts.
neighbours :: ProvinceTarget -> [ProvinceTarget]
neighbours pt1 = do
  x <- adjacency (ptProvince pt1)
  guard $ not (blacklist x pt1)
  y <- provinceTargets x
  guard $ not (blacklist (ptProvince pt1) y)
  return y

isSameOrNeighbour :: ProvinceTarget -> ProvinceTarget -> Bool
isSameOrNeighbour to from = to == from || elem to (neighbours from)

commonNeighbours :: ProvinceTarget -> ProvinceTarget -> [ProvinceTarget]
commonNeighbours pt1 pt2 =
    [ x | x <- neighbours pt1, y <- neighbours pt2, x == y ]

-- | Common neighbours which are water provinces.
commonCoasts :: ProvinceTarget -> ProvinceTarget -> [ProvinceTarget]
commonCoasts pt1 pt2 =
    filter (isWater . ptProvince) (commonNeighbours pt1 pt2)

instance Show ProvinceCoast where
  show StPetersburgNorth = "StP NC"
  show StPetersburgSouth = "StP SC"
  show SpainNorth = "Spa NC"
  show SpainSouth = "Spa SC"
  show BulgariaEast = "Bul EC"
  show BulgariaSouth = "Bul SC"

distance :: Province -> Province -> Int
distance pr1 pr2 = length (shortestPath pr1 pr2)

-- Not particularly efficient, but it should be OK...
shortestPath :: Province -> Province -> [Province]
shortestPath pr1 pr2 =
    if pr1 == pr2
    then []
    else reverse $ shortestPath' pr2 (fmap pure (adjacency pr1))
  where
    shortestPath' :: Province -> [[Province]] -> [Province]
    shortestPath' pr paths = case select pr paths of
        Just path -> path
        Nothing -> shortestPath' pr (expand paths)

    expand :: [[Province]] -> [[Province]]
    expand ps = do
        t : ts <- ps
        fmap (\x -> x : t : ts) (adjacency t)

    select :: Province -> [[Province]] -> Maybe [Province]
    select p paths = foldr select Nothing paths
      where
        select path b = b <|> if elem p path then Just path else Nothing

distanceFromHomeSupplyCentre :: GreatPower -> Province -> Int
distanceFromHomeSupplyCentre power province = head (sort distances)
  where
    distances = fmap (distance province) homeSupplyCentres
    homeSupplyCentres = filter (isHome power) supplyCentres

provinceStringRepresentation :: Province -> String
provinceStringRepresentation province = case province of
    Denmark -> "Denmark"
    Bohemia -> "Bohemia"
    Budapest -> "Budapest"
    Galicia -> "Galicia"
    Trieste -> "Trieste"
    Tyrolia -> "Tyrolia"
    Vienna -> "Vienna"
    Clyde -> "Clyde"
    Edinburgh -> "Edinburgh"
    Liverpool -> "Liverpool"
    London -> "London"
    Wales -> "Wales"
    Yorkshire -> "Yorkshire"
    Brest -> "Brest"
    Burgundy -> "Burgundy"
    Gascony -> "Gascony"
    Marseilles -> "Marseilles"
    Paris -> "Paris"
    Picardy -> "Picardy"
    Berlin -> "Berlin"
    Kiel -> "Kiel"
    Munich -> "Munich"
    Prussia -> "Prussia"
    Ruhr -> "Ruhr"
    Silesia -> "Silesia"
    Apulia -> "Apulia"
    Naples -> "Naples"
    Piedmont -> "Piedmont"
    Rome -> "Rome"
    Tuscany -> "Tuscany"
    Venice -> "Venice"
    Livonia -> "Livonia"
    Moscow -> "Moscow"
    Sevastopol -> "Sevastopol"
    StPetersburg -> "St. Petersburg"
    Ukraine -> "Ukraine"
    Warsaw -> "Warsaw"
    Ankara -> "Ankara"
    Armenia -> "Armenia"
    Constantinople -> "Constantinople"
    Smyrna -> "Smyrna"
    Syria -> "Syria"
    Albania -> "Albania"
    Belgium -> "Belgium"
    Bulgaria -> "Bulgaria"
    Finland -> "Finland"
    Greece -> "Greece"
    Holland -> "Holland"
    Norway -> "Norway"
    NorthAfrica -> "North Africa"
    Portugal -> "Portugal"
    Rumania -> "Rumania"
    Serbia -> "Serbia"
    Spain -> "Spain"
    Sweden -> "Sweden"
    Tunis -> "Tunis"
    AdriaticSea -> "Adriatic Sea"
    AegeanSea -> "Aegean Sea"
    BalticSea -> "Baltic Sea"
    BarentsSea -> "Barents Sea"
    BlackSea -> "Black Sea"
    EasternMediterranean -> "Eastern Mediterranean"
    EnglishChannel -> "English Channel"
    GulfOfBothnia -> "Gulf of Bothnia"
    GulfOfLyon -> "Gulf of Lyon"
    HeligolandBight -> "Heligoland Bight"
    IonianSea -> "Ionian Sea"
    IrishSea -> "Irish Sea"
    MidAtlanticOcean -> "Mid-Atlantic Ocean"
    NorthAtlanticOcean -> "North Atlantic Ocean"
    NorthSea -> "North Sea"
    NorwegianSea -> "Norwegian Sea"
    Skagerrak -> "Skagerrak"
    TyrrhenianSea -> "Tyrrhenian Sea"
    WesternMediterranean -> "Western Mediterranean"

provinceStringRepresentations :: Province -> (String, [String])
provinceStringRepresentations pr = (principal, others)
  where
    principal = provinceStringRepresentation pr
    others = case pr of
        Liverpool -> ["Lvp"]
        Livonia -> ["Lvn"]
        StPetersburg -> ["StP"]
        Norway -> ["Nwy"]
        NorthAfrica -> ["NAf"]
        GulfOfBothnia -> ["Bot"]
        GulfOfLyon -> ["GoL"]
        -- There are 2 accepted spellings of this one:
        --   Heligoland
        --   Helgoland
        -- according to Wikipedia.
        HeligolandBight -> ["Helgoland Bight", "Hel"]
        MidAtlanticOcean -> ["Mao", "Mid", "Mid Atlantic Ocean"]
        NorthAtlanticOcean -> ["NAt"]
        NorthSea -> ["Nth"]
        NorwegianSea -> ["Nrg"]
        TyrrhenianSea -> ["Tyn"]
        _ -> [take 3 principal]

parseProvince :: Parser Province
parseProvince = choice (longParsers ++ shortParsers)
  where
    longParsers :: [Parser Province]
    longParsers = fmap makeParser provinceLongReps
    shortParsers :: [Parser Province]
    shortParsers = fmap makeParser provinceShortReps
    provinces :: [Province]
    provinces = [minBound..maxBound]
    provinceReps :: [(Province, String, [String])]
    provinceReps = fmap reps provinces
    provinceLongReps :: [(Province, String)]
    provinceLongReps = fmap (\(pr, x, _) -> (pr, x)) provinceReps
    provinceShortReps :: [(Province, String)]
    provinceShortReps = provinceReps >>= \(pr, _, xs) -> fmap (\x -> (pr, x)) xs
    reps :: Province -> (Province, String, [String])
    reps pr = let (s, ss) = provinceStringRepresentations pr
              in  (pr, s, ss)
    makeParser :: (Province, String) -> Parser Province
    makeParser (p, s) = try (string s) *> pure p

provinceCoastStringRepresentations :: ProvinceCoast -> [String]
provinceCoastStringRepresentations pc = provinceReps >>= addSuffix
  where
    (principal, others) = provinceStringRepresentations (pcProvince pc)
    provinceReps = principal : others
    addSuffix str = [
          str ++ " " ++ suffix
        , str ++ " (" ++ suffix ++ ")"
        ]
    suffix = provinceCoastStringSuffix pc

provinceCoastStringSuffix :: ProvinceCoast -> String
provinceCoastStringSuffix pc = case pc of
    StPetersburgNorth -> "NC"
    StPetersburgSouth -> "SC"
    SpainNorth -> "NC"
    SpainSouth -> "SC"
    BulgariaEast -> "EC"
    BulgariaSouth -> "SC"

parseCoast :: Parser ProvinceCoast
parseCoast = choice parsers
  where
    parsers :: [Parser ProvinceCoast]
    parsers = fmap makeParser provinceCoastsWithReps
    provinceCoasts = [minBound..maxBound]
    provinceCoastsWithReps = fmap bundleReps provinceCoasts
    bundleReps :: ProvinceCoast -> (ProvinceCoast, [String])
    bundleReps pc = let ss = provinceCoastStringRepresentations pc
                    in  (pc, ss)
    makeParser :: (ProvinceCoast, [String]) -> Parser ProvinceCoast
    makeParser (pc, ss) = choice (fmap (try . string) ss) *> pure pc

parseProvinceTarget :: Parser ProvinceTarget
parseProvinceTarget = try parseSpecial <|> parseNormal
  where
    parseNormal = Normal <$> parseProvince
    parseSpecial = Special <$> parseCoast

provinceTargetStringRepresentation :: ProvinceTarget -> String
provinceTargetStringRepresentation pt = case pt of
    Normal p -> provinceStringRepresentation p
    Special c -> head (provinceCoastStringRepresentations c)

printProvinceTarget :: IsString a => ProvinceTarget -> a
printProvinceTarget = fromString . provinceTargetStringRepresentation

printProvince :: IsString a => Province -> a
printProvince = fromString . provinceStringRepresentation

-- | A search from a list of Provinces, via 1 or more adjacent Provinces which
--   satisfy some indicator, until another indicator is satisfied.
--   This gives simple paths from those Provinces, via Provinces which satisfy
--   the first indicator, to Provinces which satisfy the second indicator.
--
--   Example use case: convoy paths from a given Province.
--
--     convoyPaths
--         :: Occupation
--         -> Province
--         -> [(Province, [Province])]
--     convoyPaths occupation convoyingFrom =
--         fmap
--             (\(x, y, zs) -> (x, y : zs))
--             (paths (occupiedByFleet occupation) (coastalIndicator) [convoyingFrom])
--
paths
    :: (Province -> Bool)
    -> (Province -> Maybe t)
    -> [Province]
    -> [(t, Province, [Province])]
paths indicatorA indicatorB seeds = paths' [] indicatorA indicatorB (fmap (\x -> (x, [])) seeds)
  where

    paths'
        :: [(t, Province, [Province])]
        -> (Province -> Bool)
        -> (Province -> Maybe t)
        -> [(Province, [Province])]
        -> [(t, Province, [Province])]
    paths' found indicatorA indicatorB paths =
        -- At each step we take the next vanguard, but we must have the previous
        -- paths as well! Ok so why don't we just keep all of the paths?
        let nextPaths = growPaths indicatorA paths
            endpoints = takeEndpoints indicatorB nextPaths
            found' = found ++ endpoints
        in  case nextPaths of
                [] -> found'
                _ -> paths' found' indicatorA indicatorB nextPaths

    growPaths
        :: (Province -> Bool)
        -> [(Province, [Province])]
        -> [(Province, [Province])]
    growPaths indicator paths = do
        (first, theRest) <- paths
        next <- adjacency first
        let theRest' = first : theRest
        guard (not (next `elem` theRest'))
        guard (indicator next)
        return (next, theRest')

    takeEndpoints
        :: (Province -> Maybe t)
        -> [(Province, [Province])]
        -> [(t, Province, [Province])]
    takeEndpoints indicator candidates = do
        (first, rest) <- candidates
        x <- adjacency first
        case indicator x of
            Just y -> return (y, first, rest)
            Nothing -> empty
