module Diplomacy.Board (

    Board

  , emptyBoard

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

import           Diplomacy.Country
import           Diplomacy.Province
import           Diplomacy.Unit

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

type Occupy = Maybe AlignedUnit
type Control = Maybe Country

unoccupied = Nothing
uncontrolled = Nothing

occupy :: Board -> ProvinceTarget -> Maybe AlignedUnit -> Board
occupy board pt maybeUnit = Board {
    _occupy = M.insert pt maybeUnit (_occupy (clearTarget board pt))
  , _control = _control board
  }

clearTarget :: Board -> ProvinceTarget -> Board
clearTarget board pt = Board {
    _occupy = foldr combine (_occupy board) targets
  , _control = _control board
  }
    where targets = provinceTargetCluster pt
          combine next map = M.insert next Nothing map

control :: Board -> Province -> Maybe Country -> Board
control board prv maybeCountry = Board {
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
