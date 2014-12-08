module Diplomacy.Board (

    Board

  , emptyBoard
  , initialBoard

  , occupy
  , control

  , unitAt
  , controllerOf

  , occupiesProvince
  , occupies
  , controls

  , countryOccupies

  ) where

import qualified Data.Map as M
import           Control.Applicative

import           Diplomacy.Country
import           Diplomacy.Province
import           Diplomacy.Unit
import           Diplomacy.PlayerCount

-- | Description of a Diplomacy Board 
--
--   We enforce:
--     - each ProvinceTarget has at most one occupying unit.
--     - each ProvinceTarget is controlled by at most one country.
--   This includes the association between special ProvinceTargets like
--   StPetersburgNorth and their underlying province. We can never have
--   a unit in StPetersburgNorth and another in StPetersburg, for instance.
--
--   We do not enforce:
--     - unit type constraints (you can put an army in the sea).
--     - special province target constraints (you can put an army on the
--       north coast of Spain).
--   But should we enforce these?
--
data Board = Board {
    _occupy :: M.Map ProvinceTarget Occupy
  , _control :: M.Map Province Control
  } deriving (Show)

emptyBoard :: Board
emptyBoard = Board occupyMap controlMap
  where occupyMap = M.fromList $ (map (\x -> (x, unoccupied))) allProvinceTargets
        controlMap = M.fromList $ (map (\x -> (x, uncontrolled))) allProvinces

-- | Specification of the initial board layout.
--   A direct translation of https://www.wizards.com/avalonhill/rules/diplomacy.pdf
--   page 2
initialBoard :: PlayerCount -> Board
initialBoard Seven =

    occupy (Normal Vienna) austrianArmy
  . occupy (Normal Budapest) austrianArmy
  . occupy (Normal Trieste) austrianFleet
  . control Vienna (Just Austria)
  . control Budapest (Just Austria)
  . control Trieste (Just Austria)

  . occupy (Normal London) englishFleet
  . occupy (Normal Edinburgh) englishFleet
  . occupy (Normal Liverpool) englishArmy
  . control London (Just UnitedKingdom)
  . control Edinburgh (Just UnitedKingdom)
  . control Liverpool (Just UnitedKingdom)

  . occupy (Normal Paris) frenchArmy
  . occupy (Normal Marseilles) frenchArmy
  . occupy (Normal Brest) frenchFleet
  . control Paris (Just France)
  . control Marseilles (Just France)
  . control Brest (Just France)

  . occupy (Normal Berlin) germanArmy
  . occupy (Normal Munich) germanArmy
  . occupy (Normal Kiel) germanFleet
  . control Berlin (Just Germany)
  . control Munich (Just Germany)
  . control Kiel (Just Germany)

  . occupy (Normal Rome) italianArmy
  . occupy (Normal Venice) italianArmy
  . occupy (Normal Naples) italianFleet
  . control Rome (Just Italy)
  . control Venice (Just Italy)
  . control Naples (Just Italy)

  . occupy (Normal Moscow) russianArmy
  . occupy (Normal Sevastopol) russianFleet
  . occupy (Normal Warsaw) russianArmy
  . occupy (Special StPetersburgWest) russianFleet
  . control Moscow (Just Russia)
  . control Sevastopol (Just Russia)
  . control Warsaw (Just Russia)
  . control (pcProvince StPetersburgWest) (Just Russia)

  . occupy (Normal Ankara) turkishFleet
  . occupy (Normal Constantinople) turkishArmy
  . occupy (Normal Smyrna) turkishArmy
  . control Ankara (Just Ottoman)
  . control Constantinople (Just Ottoman)
  . control Smyrna (Just Ottoman)

  $ emptyBoard
    where austrianArmy = Just (align army Austria)
          austrianFleet = Just (align fleet Austria)
          englishArmy = Just (align army UnitedKingdom)
          englishFleet = Just (align fleet UnitedKingdom)
          frenchArmy = Just (align army France)
          frenchFleet = Just (align fleet France)
          germanArmy = Just (align army Germany)
          germanFleet = Just (align fleet Germany)
          italianArmy = Just (align army Italy)
          italianFleet = Just (align fleet Italy)
          russianArmy = Just (align army Russia)
          russianFleet = Just (align fleet Russia)
          turkishArmy = Just (align army Ottoman)
          turkishFleet = Just (align fleet Ottoman)

type Occupy = Maybe AlignedUnit
type Control = Maybe Country

unoccupied = Nothing
uncontrolled = Nothing

occupy :: ProvinceTarget -> Maybe AlignedUnit -> Board -> Board
occupy pt maybeUnit board = Board {
    _occupy = M.insert pt maybeUnit (_occupy (clearTarget pt board))
  , _control = _control board
  }

clearTarget :: ProvinceTarget -> Board -> Board
clearTarget pt board = Board {
    _occupy = foldr combine (_occupy board) targets
  , _control = _control board
  }
    where targets = provinceTargetCluster pt
          combine next map = M.insert next Nothing map

control :: Province -> Maybe Country -> Board -> Board
control prv maybeCountry board = Board {
    _control = M.insert prv maybeCountry (_control board)
  , _occupy = _occupy board
  }

unitAt :: Board -> ProvinceTarget -> Maybe AlignedUnit
unitAt board pt = maybe Nothing id (M.lookup pt (_occupy board))
-- Note that M.lookup will never be Nothing, since we never clear any keys and
-- the only injection into Board is via emptyBoard, which defines a key for all
-- of Province, ProvinceTarget.

controllerOf :: Board -> Province -> Maybe Country
controllerOf board prv = maybe Nothing id (M.lookup prv (_control board))

occupiesProvince :: Board -> AlignedUnit -> Province -> Bool
occupiesProvince b u prv = any (occupies b u) (provinceTargets prv)

occupies :: Board -> AlignedUnit -> ProvinceTarget -> Bool
occupies b u pt = maybe False ((==) u) (unitAt b pt)

controls :: Board -> Country -> Province -> Bool
controls b c p = maybe False ((==) c) (controllerOf b p)

countryOccupies :: Board -> Country -> ProvinceTarget -> Bool
countryOccupies b c pt = maybe False (((==) c) . alignedCountry) (unitAt b pt)
