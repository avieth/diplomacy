{-# LANGUAGE GADTs #-}

module Diplomacy.Board (

    Board

  , Typical
  , Retreat
  , Adjust

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

  , occupiedProvinceTargets

  , unitCount

  , supplyCentreCount

  , dislodged

  ) where

import qualified Data.Map as M
import           Control.Applicative

import           Diplomacy.Country
import           Diplomacy.Province
import           Diplomacy.Unit
import           Diplomacy.PlayerCount
import           Diplomacy.Phase (Typical, Retreat, Adjust)

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
--   TODO should parameterize on some type 
--     Retreat | Typical
--   so that we can identify board which potentially have 2 units on the same
--   place, one of which is displaced. We could have:
--
--   data Board a where
--     TypicalBoard :: M.Map ProvinceTarget Occupy -> M.Map Province Control -> Board Typical
--     RetreatBoard :: M.Map ProvinceTarget Occupy -> M.Map ProvinceTarget Dislodged -> M.Map Province Control -> Board Retreat
--
data Board a where

  TypicalBoard :: M.Map ProvinceTarget Occupy
                  -- ^ Occupation of province targets.
               -> M.Map Province Control
                  -- ^ Control of provinces.
               -> Board Typical

  RetreatBoard :: M.Map ProvinceTarget Occupy
                  -- ^ Occupation of province targets.
               -> M.Map Province Control
                  -- ^ Control of provinces.
               -> M.Map ProvinceTarget Dislodge
                  -- ^ Dislodged units.
               -> Board Retreat

  AdjustBoard  :: M.Map ProvinceTarget Occupy
                  -- ^ Occupation of province targets.
               -> M.Map Province Control
                  -- ^ Control of provinces.
               -> Board Adjust

_occupy :: Board a -> M.Map ProvinceTarget Occupy
_occupy (TypicalBoard o _) = o
_occupy (RetreatBoard o _ _) = o
_occupy (AdjustBoard o _) = o

_control :: Board a -> M.Map Province Control
_control (TypicalBoard _ c) = c
_control (RetreatBoard _ c _) = c
_control (AdjustBoard _ c) = c

_dislodge :: Board Retreat -> M.Map ProvinceTarget Dislodge
_dislodge (RetreatBoard _ _ d) = d

updateBoard :: M.Map ProvinceTarget Occupy -> M.Map Province Control -> Board a -> Board a
updateBoard o c (TypicalBoard _ _) = TypicalBoard o c
updateBoard o c (RetreatBoard _ _ d) = RetreatBoard o c d
updateBoard o c (AdjustBoard _ _) = AdjustBoard o c

emptyBoard :: Board Typical
emptyBoard = TypicalBoard occupyMap controlMap
  where occupyMap = M.fromList $ (map (\x -> (x, unoccupied))) allProvinceTargets
        controlMap = M.fromList $ (map (\x -> (x, uncontrolled))) allProvinces

-- | Specification of the initial board layout.
--   A direct translation of https://www.wizards.com/avalonhill/rules/diplomacy.pdf
--   page 2
initialBoard :: PlayerCount -> Board Typical
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
type Dislodge = Maybe AlignedUnit

unoccupied = Nothing
uncontrolled = Nothing
nodislodge = Nothing

occupy :: ProvinceTarget -> Maybe AlignedUnit -> Board a -> Board a
occupy pt maybeUnit board = updateBoard newOccupy newControl board
  where newOccupy = M.insert pt maybeUnit (_occupy (clearTarget pt board))
        newControl = _control board

clearTarget :: ProvinceTarget -> Board a -> Board a
clearTarget pt board = updateBoard newOccupy newControl board
  where newOccupy = foldr combine (_occupy board) targets
        newControl =_control board
        targets = provinceTargetCluster pt
        combine next map = M.insert next Nothing map

control :: Province -> Maybe Country -> Board a -> Board a
control prv maybeCountry board = updateBoard newOccupy newControl board
  where newControl = M.insert prv maybeCountry (_control board)
        newOccupy = _occupy board

unitAt :: Board a -> ProvinceTarget -> Maybe AlignedUnit
unitAt board pt = maybe Nothing id (M.lookup pt (_occupy board))
-- Note that M.lookup will never be Nothing, since we never clear any keys and
-- the only injection into Board is via emptyBoard, which defines a key for all
-- of Province, ProvinceTarget.

controllerOf :: Board a -> Province -> Maybe Country
controllerOf board prv = maybe Nothing id (M.lookup prv (_control board))

occupiesProvince :: Board a -> AlignedUnit -> Province -> Bool
occupiesProvince b u prv = any (occupies b u) (provinceTargets prv)

occupies :: Board a -> AlignedUnit -> ProvinceTarget -> Bool
occupies b u pt = maybe False ((==) u) (unitAt b pt)

controls :: Board a -> Country -> Province -> Bool
controls b c p = maybe False ((==) c) (controllerOf b p)

countryOccupies :: Board a -> Country -> ProvinceTarget -> Bool
countryOccupies b c pt = maybe False (((==) c) . alignedCountry) (unitAt b pt)

occupiedProvinceTargets :: Board a -> [(ProvinceTarget, AlignedUnit)]
occupiedProvinceTargets b = M.foldrWithKey select [] (_occupy b)
  where select pt Nothing xs = xs
        select pt (Just u) xs = (pt, u) : xs

-- | Number of supply centres countrolled by a given power.
supplyCentreCount :: Country -> Board a -> Int
supplyCentreCount c b = M.foldrWithKey count 0 (_control b)
  where count pr control t =
          if supplyCentre pr
          then case control of
                 Nothing -> t
                 Just c' -> if c' == c then t + 1 else t
          else t

-- | We wish to count the number of units on the board belonging to a given
--   Country, but since this is done differently for a Retreat board (must
--   check the dislodged units) we use a typeclass to accomplish this
--   transparently.
class UnitCount phaseType where
  unitCount :: Country -> Board phaseType -> Int

instance UnitCount Typical where
  unitCount c b = M.foldr count 0 (_occupy b)
    where count unit t = maybe t (\u -> if alignedCountry u == c then t + 1 else t) unit

instance UnitCount Retreat where
  unitCount c b = (M.foldr count 0 (_occupy b)) + (M.foldr count 0 (_dislodge b))
    where count unit t = maybe t (\u -> if alignedCountry u == c then t + 1 else t) unit

instance UnitCount Adjust where
  unitCount c b = M.foldr count 0 (_occupy b)
    where count unit t = maybe t (\u -> if alignedCountry u == c then t + 1 else t) unit

-- | Get the dislodged unit, if any, at a given ProvinceTarget.
dislodged :: Board Retreat -> ProvinceTarget -> Maybe AlignedUnit
dislodged brd pt = maybe Nothing id (M.lookup pt (_dislodge brd))
