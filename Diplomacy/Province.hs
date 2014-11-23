module Diplomacy.Province (

    Province(..)
  , allProvinces

  , adjacent

  , ProvinceType(..)
  , provinceType

  , Territory
  , territory

  -- Verification
  , reflexivityCheck
  , antisymmetryCheck

  ) where

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
adjacent :: Province -> [Province]
adjacent Bohemia = [Munich, Tyrolia, Vienna, Silesia, Galacia]
adjacent Budapest = [Vienna, Galacia, Rumania, Serbia, Trieste]
adjacent Galacia = [Warsaw, Silesia, Ukraine, Rumania, Budapest, Vienna, Bohemia]
adjacent Trieste = [AdriaticSea, Venice, Tyrolia, Vienna, Budapest, Serbia, Albania]
adjacent Tyrolia = [Piedmont, Munich, Bohemia, Vienna, Trieste, Venice]
adjacent Vienna = [Trieste, Tyrolia, Bohemia, Galacia, Budapest]
adjacent Clyde = [NorthAtlanticOcean, NorwegianSea, Edinburgh, Liverpool]
adjacent Edinburgh = [Clyde, NorwegianSea, NorthSea, Yorkshire, Liverpool]
adjacent Liverpool = [NorthAtlanticOcean, IrishSea, Clyde, Edinburgh, Yorkshire, Wales]
adjacent London = [NorthSea, EnglishChannel, Wales, Yorkshire]
adjacent Wales = [IrishSea, EnglishChannel, London, Yorkshire, Liverpool]
adjacent Yorkshire = [Liverpool, Edinburgh, London, Wales, NorthSea]
adjacent Brest = [EnglishChannel, MidAtlanticOcean, Picardy, Paris, Gascony]
adjacent Burgundy = [Paris, Picardy, Belgium, Ruhr, Munich, Marseilles, Gascony]
adjacent Gascony = [MidAtlanticOcean, Spain, Brest, Paris, Burgundy, Marseilles]
adjacent Marseilles = [GulfOfLyon, Spain, Gascony, Burgundy, Piedmont]
adjacent Paris = [Brest, Picardy, Burgundy, Gascony]
adjacent Picardy = [EnglishChannel, Belgium, Burgundy, Paris, Brest]
adjacent Berlin = [BalticSea, Prussia, Silesia, Munich, Kiel]
-- TODO verify this one!
adjacent Kiel = [HelgolandBright, Berlin, Munich, Ruhr, Holland, Denmark, BalticSea]
adjacent Munich = [Ruhr, Kiel, Berlin, Silesia, Bohemia, Tyrolia, Burgundy]
adjacent Prussia = [BalticSea, Livonia, Warsaw, Silesia, Berlin]
adjacent Ruhr = [Belgium, Holland, Kiel, Munich, Burgundy]
adjacent Silesia = [Munich, Berlin, Prussia, Warsaw, Galacia, Bohemia]
adjacent Apulia = [AdriaticSea, IonianSea, Naples, Rome, Venice]
adjacent Naples = [IonianSea, TyrrhenianSea, Apulia, Rome]
adjacent Piedmont = [Marseilles, Tyrolia, GulfOfLyon, Venice, Tuscany]
adjacent Rome = [TyrrhenianSea, Naples, Tuscany, Venice, Apulia]
adjacent Tuscany = [GulfOfLyon, Piedmont, Venice, Rome, TyrrhenianSea]
adjacent Venice = [Piedmont, Tyrolia, Trieste, AdriaticSea, Apulia, Tuscany, Rome]
adjacent Livonia = [BalticSea, GulfOfBothnia, StPetersburg, Moscow, Warsaw, Prussia]
adjacent Moscow = [StPetersburg, Sevastopol, Ukraine, Warsaw, Livonia]
adjacent Sevastopol = [Armenia, BlackSea, Rumania, Ukraine, Moscow]
adjacent StPetersburg = [BarentsSea, Moscow, Livonia, GulfOfBothnia, Finland]
adjacent Ukraine = [Moscow, Sevastopol, Rumania, Galacia, Warsaw]
adjacent Warsaw = [Prussia, Livonia, Moscow, Ukraine, Galacia, Silesia]
adjacent Ankara = [BlackSea, Armenia, Smyrna, Constantinople]
adjacent Armenia = [BlackSea, Sevastopol, Syria, Ankara, Smyrna]
adjacent Constantinople = [BlackSea, Ankara, Smyrna, Bulgaria, AegeanSea]
adjacent Smyrna = [EasternMediterranean, AegeanSea, Constantinople, Ankara, Armenia, Syria]
adjacent Syria = [Armenia, Smyrna, EasternMediterranean]
adjacent Albania = [AdriaticSea, Trieste, Serbia, Greece, IonianSea]
adjacent Belgium = [Holland, Ruhr, Burgundy, Picardy, EnglishChannel, NorthSea]
adjacent Bulgaria = [Rumania, BlackSea, Constantinople, AegeanSea, Greece, Serbia]
adjacent Finland = [StPetersburg, Sweden, Norway, GulfOfBothnia]
adjacent Greece = [IonianSea, AegeanSea, Albania, Serbia, Bulgaria]
adjacent Holland = [Belgium, NorthSea, Kiel, Ruhr, HelgolandBright]
adjacent Norway = [NorwegianSea, NorthSea, Sweden, Finland, Skagerrak, BarentsSea]
adjacent NorthAfrica = [MidAtlanticOcean, WesternMediterranean, Tunis]
adjacent Portugal = [MidAtlanticOcean, Spain]
adjacent Rumania = [BlackSea, Bulgaria, Serbia, Budapest, Galacia, Ukraine, Sevastopol]
adjacent Serbia = [Trieste, Budapest, Rumania, Bulgaria, Greece, Albania]
adjacent Spain = [Portugal, MidAtlanticOcean, Gascony, GulfOfLyon, WesternMediterranean, Marseilles]
adjacent Sweden = [GulfOfBothnia, Finland, Norway, BalticSea, Skagerrak, Denmark]
adjacent Tunis = [NorthAfrica, WesternMediterranean, IonianSea, TyrrhenianSea]
adjacent Denmark = [BalticSea, Skagerrak, HelgolandBright, Kiel, NorthSea, Sweden]
adjacent AdriaticSea = [Trieste, Venice, Apulia, Albania, IonianSea]
adjacent AegeanSea = [Greece, Bulgaria, Constantinople, Smyrna, EasternMediterranean, IonianSea]
adjacent BalticSea = [Sweden, GulfOfBothnia, Livonia, Prussia, Berlin, Kiel, Denmark]
adjacent BarentsSea = [StPetersburg, Norway, NorwegianSea]
adjacent BlackSea = [Sevastopol, Armenia, Ankara, Constantinople, Bulgaria, Rumania]
adjacent EasternMediterranean = [Syria, IonianSea, AegeanSea, Smyrna]
adjacent EnglishChannel = [London, Belgium, Picardy, Brest, MidAtlanticOcean, IrishSea, Wales, NorthSea]
adjacent GulfOfBothnia = [Sweden, Finland, Livonia, StPetersburg, BalticSea]
adjacent GulfOfLyon = [Marseilles, Piedmont, Tuscany, TyrrhenianSea, WesternMediterranean, Spain]
adjacent HelgolandBright = [Denmark, Kiel, Holland, NorthSea]
adjacent IonianSea = [Tunis, TyrrhenianSea, Naples, Apulia, AdriaticSea, Greece, Albania, AegeanSea, EasternMediterranean]
adjacent IrishSea = [NorthAtlanticOcean, EnglishChannel, MidAtlanticOcean, Liverpool, Wales]
adjacent MidAtlanticOcean = [NorthAtlanticOcean, IrishSea, EnglishChannel, Brest, Gascony, Spain, Portugal, WesternMediterranean, NorthAfrica]
adjacent NorthAtlanticOcean = [NorwegianSea, Clyde, Liverpool, IrishSea, MidAtlanticOcean]
adjacent NorthSea = [NorwegianSea, Skagerrak, Denmark, HelgolandBright, Holland, Belgium, EnglishChannel, London, Yorkshire, Edinburgh, Norway]
adjacent NorwegianSea = [NorthAtlanticOcean, Norway, BarentsSea, NorthSea, Clyde, Edinburgh]
adjacent Skagerrak = [Norway, Sweden, Denmark, NorthSea]
adjacent TyrrhenianSea = [GulfOfLyon, WesternMediterranean, Tunis, Tuscany, Rome, Naples, IonianSea]
adjacent WesternMediterranean = [NorthAfrica, MidAtlanticOcean, GulfOfLyon, Spain, Tunis, TyrrhenianSea]

allProvinces :: [Province]
allProvinces = [minBound .. maxBound]

-- | A list of all pairs for which adjacent is not reflexive.
reflexivityCheck :: [(Province, Province)]
reflexivityCheck = filter (not . adjacencyIsReflexive) (pairs allProvinces)

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs l@(x:xs) = (map ((,) x) l) ++ (pairs xs)

adjacencyIsReflexive :: (Province, Province) -> Bool
adjacencyIsReflexive (p1, p2) =
  if p1 `elem` adjacent p2
  then p2 `elem` adjacent p1
  else not $ p2 `elem` adjacent p1

-- | A list of all pairs for which adjacent is symmetric (we want it to be
--   antisymmetric).
antisymmetryCheck :: [Province]
antisymmetryCheck = filter adjacencyIsSymmetric allProvinces

adjacencyIsSymmetric :: Province -> Bool
adjacencyIsSymmetric province = province `elem` adjacent province

-- | Description of a place on the board.
--   The only values of this type shall correspond to the Province enumeration
data Territory = Territory Province ProvinceType
  deriving (Eq, Ord, Show)

-- | Injection into Territory; just pairs with the province type.
territory :: Province -> Territory
territory province = Territory province (provinceType province)
