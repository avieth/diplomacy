module Diplomacy.Province (

    Province(..)
  , allProvinces

  , adjacency
  , adjacent

  , convoyAdjacent
  , moveAdjacent

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

  ) where

import Diplomacy.Country

-- | Exhaustive enumeration of the places on the diplomacy board.
--   Refernce: https://www.wizards.com/avalonhill/rules/diplomacy.pdf
data Province
  = Bohemia
  | Budapest
  | Galacia
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
  | HelgolandBright
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
provinceType Galacia = Inland
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
provinceType HelgolandBright = Water
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
adjacency Bohemia = [Munich, Tyrolia, Vienna, Silesia, Galacia]
adjacency Budapest = [Vienna, Galacia, Rumania, Serbia, Trieste]
adjacency Galacia = [Warsaw, Silesia, Ukraine, Rumania, Budapest, Vienna, Bohemia]
adjacency Trieste = [AdriaticSea, Venice, Tyrolia, Vienna, Budapest, Serbia, Albania]
adjacency Tyrolia = [Piedmont, Munich, Bohemia, Vienna, Trieste, Venice]
adjacency Vienna = [Trieste, Tyrolia, Bohemia, Galacia, Budapest]
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
adjacency Kiel = [HelgolandBright, Berlin, Munich, Ruhr, Holland, Denmark, BalticSea]
adjacency Munich = [Ruhr, Kiel, Berlin, Silesia, Bohemia, Tyrolia, Burgundy]
adjacency Prussia = [BalticSea, Livonia, Warsaw, Silesia, Berlin]
adjacency Ruhr = [Belgium, Holland, Kiel, Munich, Burgundy]
adjacency Silesia = [Munich, Berlin, Prussia, Warsaw, Galacia, Bohemia]
adjacency Apulia = [AdriaticSea, IonianSea, Naples, Rome, Venice]
adjacency Naples = [IonianSea, TyrrhenianSea, Apulia, Rome]
adjacency Piedmont = [Marseilles, Tyrolia, GulfOfLyon, Venice, Tuscany]
adjacency Rome = [TyrrhenianSea, Naples, Tuscany, Venice, Apulia]
adjacency Tuscany = [GulfOfLyon, Piedmont, Venice, Rome, TyrrhenianSea]
adjacency Venice = [Piedmont, Tyrolia, Trieste, AdriaticSea, Apulia, Tuscany, Rome]
adjacency Livonia = [BalticSea, GulfOfBothnia, StPetersburg, Moscow, Warsaw, Prussia]
adjacency Moscow = [StPetersburg, Sevastopol, Ukraine, Warsaw, Livonia]
adjacency Sevastopol = [Armenia, BlackSea, Rumania, Ukraine, Moscow]
adjacency StPetersburg = [BarentsSea, Moscow, Livonia, GulfOfBothnia, Finland]
adjacency Ukraine = [Moscow, Sevastopol, Rumania, Galacia, Warsaw]
adjacency Warsaw = [Prussia, Livonia, Moscow, Ukraine, Galacia, Silesia]
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
adjacency Holland = [Belgium, NorthSea, Kiel, Ruhr, HelgolandBright]
adjacency Norway = [NorwegianSea, NorthSea, Sweden, Finland, Skagerrak, BarentsSea]
adjacency NorthAfrica = [MidAtlanticOcean, WesternMediterranean, Tunis]
adjacency Portugal = [MidAtlanticOcean, Spain]
adjacency Rumania = [BlackSea, Bulgaria, Serbia, Budapest, Galacia, Ukraine, Sevastopol]
adjacency Serbia = [Trieste, Budapest, Rumania, Bulgaria, Greece, Albania]
adjacency Spain = [Portugal, MidAtlanticOcean, Gascony, GulfOfLyon, WesternMediterranean, Marseilles]
adjacency Sweden = [GulfOfBothnia, Finland, Norway, BalticSea, Skagerrak, Denmark]
adjacency Tunis = [NorthAfrica, WesternMediterranean, IonianSea, TyrrhenianSea]
adjacency Denmark = [BalticSea, Skagerrak, HelgolandBright, Kiel, NorthSea, Sweden]
adjacency AdriaticSea = [Trieste, Venice, Apulia, Albania, IonianSea]
adjacency AegeanSea = [Greece, Bulgaria, Constantinople, Smyrna, EasternMediterranean, IonianSea]
adjacency BalticSea = [Sweden, GulfOfBothnia, Livonia, Prussia, Berlin, Kiel, Denmark]
adjacency BarentsSea = [StPetersburg, Norway, NorwegianSea]
adjacency BlackSea = [Sevastopol, Armenia, Ankara, Constantinople, Bulgaria, Rumania]
adjacency EasternMediterranean = [Syria, IonianSea, AegeanSea, Smyrna]
adjacency EnglishChannel = [London, Belgium, Picardy, Brest, MidAtlanticOcean, IrishSea, Wales, NorthSea]
adjacency GulfOfBothnia = [Sweden, Finland, Livonia, StPetersburg, BalticSea]
adjacency GulfOfLyon = [Marseilles, Piedmont, Tuscany, TyrrhenianSea, WesternMediterranean, Spain]
adjacency HelgolandBright = [Denmark, Kiel, Holland, NorthSea]
adjacency IonianSea = [Tunis, TyrrhenianSea, Naples, Apulia, AdriaticSea, Greece, Albania, AegeanSea, EasternMediterranean]
adjacency IrishSea = [NorthAtlanticOcean, EnglishChannel, MidAtlanticOcean, Liverpool, Wales]
adjacency MidAtlanticOcean = [NorthAtlanticOcean, IrishSea, EnglishChannel, Brest, Gascony, Spain, Portugal, WesternMediterranean, NorthAfrica]
adjacency NorthAtlanticOcean = [NorwegianSea, Clyde, Liverpool, IrishSea, MidAtlanticOcean]
adjacency NorthSea = [NorwegianSea, Skagerrak, Denmark, HelgolandBright, Holland, Belgium, EnglishChannel, London, Yorkshire, Edinburgh, Norway]
adjacency NorwegianSea = [NorthAtlanticOcean, Norway, BarentsSea, NorthSea, Clyde, Edinburgh]
adjacency Skagerrak = [Norway, Sweden, Denmark, NorthSea]
adjacency TyrrhenianSea = [GulfOfLyon, WesternMediterranean, Tunis, Tuscany, Rome, Naples, IonianSea]
adjacency WesternMediterranean = [NorthAfrica, MidAtlanticOcean, GulfOfLyon, Spain, Tunis, TyrrhenianSea]

adjacent :: Province -> Province -> Bool
adjacent prv0 prv1 = prv0 `elem` (adjacency prv1)

allProvinces :: [Province]
allProvinces = [minBound .. maxBound]

-- | A list of all pairs for which adjacency is symmetric
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
country :: Province -> Maybe Country
country Bohemia = Just Austria
country Budapest = Just Austria
country Galacia = Just Austria
country Trieste = Just Austria
country Tyrolia = Just Austria
country Vienna = Just Austria
country Clyde = Just UnitedKingdom
country Edinburgh = Just UnitedKingdom
country Liverpool = Just UnitedKingdom
country London = Just UnitedKingdom
country Wales = Just UnitedKingdom
country Yorkshire = Just UnitedKingdom
country Brest = Just France
country Burgundy = Just France
country Gascony = Just France
country Marseilles = Just France
country Paris = Just France
country Picardy = Just Germany
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
country Ankara = Just Ottoman
country Armenia = Just Ottoman
country Constantinople = Just Ottoman
country Smyrna = Just Ottoman
country Syria = Just Ottoman
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
country HelgolandBright = Nothing
country IonianSea = Nothing
country IrishSea = Nothing
country MidAtlanticOcean = Nothing
country NorthAtlanticOcean = Nothing
country NorthSea = Nothing
country NorwegianSea = Nothing
country Skagerrak = Nothing
country TyrrhenianSea = Nothing
country WesternMediterranean = Nothing

isHome :: Country -> Province -> Bool
isHome c p = maybe False ((==) c) (country p)

-- | Province does not express all move order targets, like the north coast
--   of StPetersburg.
data ProvinceTarget
  = Normal Province
  | Special ProvinceCoast
    deriving (Eq, Ord, Show)

isSpecial :: ProvinceTarget -> Bool
isSpecial (Special _) = True
isSpecial _ = False

isNormal :: ProvinceTarget -> Bool
isNormal (Normal _) = True
isNormal _ = False

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
  | StPetersburgWest
  | SpainNorth
  | SpainSouth
  | BulgariaEast
  | BulgariaSouth
    deriving (Eq, Ord, Show)

pcProvince :: ProvinceCoast -> Province
pcProvince StPetersburgNorth = StPetersburg
pcProvince StPetersburgWest = StPetersburg
pcProvince SpainNorth = Spain
pcProvince SpainSouth = Spain
pcProvince BulgariaEast = Bulgaria
pcProvince BulgariaSouth = Bulgaria

provinceCoasts :: Province -> [ProvinceCoast]
provinceCoasts StPetersburg = [StPetersburgNorth, StPetersburgWest]
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

-- | Valid moves do not correspond exactly to adjacent (incl. convoy) provinces.
--   We account here for the special cases involving coastlines.
--   See https://www.wizards.com/avalonhill/rules/diplomacy.pdf page 5
--   The Kiel/Constantinople and Sweden/Denmark clarifications are already
--   defined in the Province adjacency; we need only look after the split
--   coastlines in StPetersburg, Bulgaria, and Spain.
moveAdjacent :: ProvinceTarget -> ProvinceTarget -> Bool
-- Easiest case first: convince yourself that special coastline is not
-- reachable from a special coastline. This is the whole point of special
-- coastlines: a unit on one can never move immediately to another; it must
-- go through water provinces.
moveAdjacent (Special _) (Special _) = False
-- A special province target is never reached by convoy, so we can use adjacent
-- to determine this. A move to a special target must be from a non-inland
-- province target (subtle: only fleets can move to special coastlines, and
-- fleets will never be inland), and we use the blacklist function to take
-- care of ad-hoc special cases presented in the rulebook.
moveAdjacent pt0 (Special pc) =
    let prv0 = ptProvince pt0
    in (not (blacklist prv0 pc)) && (not (isInland prv0)) && ((pcProvince pc) `adjacent` prv0)
-- Surely reachability is symmetric!
moveAdjacent (Special pc) pt0 = moveAdjacent pt0 (Special pc)
-- The fall-through case: just use the Province adjacency definition, eliminating
-- a convoy-adjacent move for fleets.
moveAdjacent (Normal prv0) (Normal prv1) = prv0 `adjacent` prv1

convoyAdjacent :: ProvinceTarget -> ProvinceTarget -> Bool
convoyAdjacent (Special _) _ = False
convoyAdjacent _ (Special _) = False
convoyAdjacent (Normal prv0) (Normal prv1) = convoyAdjacent' prv0 prv1

-- | Adjacent via convoy iff there is a nontrivial noncyclic path over water
--   provinces from first province to second.
convoyAdjacent' :: Province -> Province -> Bool
convoyAdjacent' prv0 prv1
    | prv0 == prv1 = False
    | otherwise = convoyAdjacent'' [] prv0 prv1
  where
    -- Must use the 'used' name to track provinces already visited, so that we don't
    -- get caught in loops in our adjacency function.
    convoyAdjacent'' used prv0 prv1 =
      let waterNeighbours = filter (\x -> isWater x && (not (elem x used))) (adjacency prv0)
      in any (\x -> (x `adjacent` prv1) || (convoyAdjacent'' (x : used) x prv1)) waterNeighbours

-- | True iff the given province should not be considered adjacent to the
--   given province coast.
blacklist :: Province -> ProvinceCoast -> Bool
blacklist WesternMediterranean SpainNorth = True
-- NB MidAtlanticOcean to SpainSouth is fine!
blacklist GulfOfBothnia StPetersburgNorth = True
blacklist BarentsSea StPetersburgWest = True
blacklist BlackSea BulgariaSouth = True
blacklist AegeanSea BulgariaEast = True
blacklist _ _ = False
