{-# LANGUAGE GADTs #-}

module Diplomacy.Board (

    Board

  , initialBoard

  -- Functions for "advancing" a board from phase to phase.
  , makeRetreat
  , makeAdjust
  , makeTypical
  , endAdjustment

  , giveOrder
  , orders

  , DiplomacyMap
  , diplomacyMap

  , unitAt
  , controllerOf
  , dislodged
  , occupies
  , controls

  , unitCount
  , supplyCentreCount

  , supplyCentreSurplusses
  , supplyCentreDefecits

  ) where

import qualified Data.Map as M
import           Control.Applicative

import           Diplomacy.Phase (Typical, Retreat, Adjust)
import           Diplomacy.Country
import           Diplomacy.Province
import           Diplomacy.Unit
import           Diplomacy.PlayerCount
import           Diplomacy.Order
import           Diplomacy.OrderInvalid
import           Diplomacy.ResolvedOrders

-- | Values of type Board phaseType capture the state of a diplomacy game at
--   a phase of that type (we use a phase type rather than a phase because
--   there is no difference in game state form between Spring/Autumn and
--   SpringRetreat/AutumnRetreat). This includes: occupation of province
--   targets, control of countries, orders for each occupying unit in a typical
--   phase, each dislodged unit and its order in a retreat phase, and a set of
--   adjustment orders in an adjustment phase.
data Board phaseType where

  TypicalBoard
    :: M.Map ProvinceTarget (AlignedUnitAndOrderObject Typical)
    -- ^ In a typical board, each ProvinceTarget which is
    -- occupied determines an occupying country, a unit, and
    -- an order. This gives us a static guarantee that every
    -- unit in a typical phase has an order, and that's a good
    -- thing.
    -> M.Map Province Country
    -- ^ Control of provinces.
    -> Board Typical

  RetreatBoard
    :: M.Map ProvinceTarget AlignedUnit
    -- ^ Occupation of province targets. Note that on a retreat
    -- board there is no associated order. Instead, we put the
    -- orders on the dislodged units.
    -> M.Map Province Country
    -- ^ Control of provinces.
    -> M.Map ProvinceTarget (AlignedUnitAndOrderObject Retreat)
    -- ^ Dislodged units. Each one has an order.
    -> ResolvedOrders Typical
    -- ^ Resolved orders from the previous phase; they are necessary to
    -- determine validity of retreat phase orders.
    -> Board Retreat

  AdjustBoard
    :: M.Map ProvinceTarget AlignedUnit
    -- ^ Occupation of province targets. There is no associated
    -- order, because in the adjustment phase, no units require
    -- orders. We handle unit vs. supply centre count in the
    -- NormalizedBoard datatype.
    -> M.Map Province Country
    -- ^ Control of provinces.
    -> M.Map ProvinceTarget (AlignedUnitAndOrderObject Adjust)
    -- ^ Track the adjustment orders. This one is tricky because the adjustment
    -- order subject interpretation is different depending on the order: for
    -- a build it means make this (not yet existing) unit at this province
    -- target, but for a disband it means remove this (existing) unit at this
    -- province target.
    --
    -- A note on validity:
    --   - cannot disband and build at the same province target in one adjust
    --     phase; i.e. you can't swap a fleet for an army by disbanding then
    --     building.
    --   - must specify a ProvinceTarget, not just a Province.
    --   - must be in home country
    --   - must not cause a defecit
    --   - cannot build a fleet inland
    --
    -> Board Adjust

-- | A DiplomacyMap is a Board without any orders. The only way to create one
--   is from a Board. It contains the information about a board which every
--   player is allowed to know.
data DiplomacyMap phaseType where

  TypicalMap
    :: M.Map ProvinceTarget AlignedUnit
    -> M.Map Province Country
    -> DiplomacyMap Typical

  RetreatMap
    :: M.Map ProvinceTarget AlignedUnit
    -> M.Map Province Country
    -> M.Map ProvinceTarget AlignedUnit
    -> DiplomacyMap Retreat

  AdjustMap
    :: M.Map ProvinceTarget AlignedUnit
    -> M.Map Province Country
    -> DiplomacyMap Adjust

diplomacyMap :: Board phaseType -> DiplomacyMap phaseType
diplomacyMap (TypicalBoard o c) = TypicalMap (M.map dropOrderObject o) c
diplomacyMap (RetreatBoard o c d _) = RetreatMap o c (M.map dropOrderObject d)
diplomacyMap (AdjustBoard o c _) = AdjustMap o c

-- | Give an order. If the order is invalid (cannot possible be fulfilled) then
--   we give the reason why.
giveOrder
  :: Order phaseType
  -> Board phaseType
  -> Either (OrderInvalid phaseType) (Board phaseType)
giveOrder ord (TypicalBoard o c) = undefined
giveOrder ord (RetreatBoard o c d res) = undefined
giveOrder ord (AdjustBoard o c ords) = undefined

-- | TODO this needs attention. Should be more concise, and not duplicate the
--   big where clause.
orders :: Country -> Board phaseType -> [Order phaseType]
orders country (TypicalBoard brd _) = M.foldrWithKey selectOrder [] brd
  where
    selectOrder pt auo ords =
        let au = dropOrderObject auo
            u = alignedUnit au
            country' = alignedCountry au
            oo = dropAlignedUnit (auo)
            subject = OrderSubject u pt
        in if country' == country
           then makeOrder country subject oo : ords
           else ords
orders country (RetreatBoard _ _ d _) = M.foldrWithKey selectOrder [] d
  where
    selectOrder pt auo ords =
        let au = dropOrderObject auo
            u = alignedUnit au
            country' = alignedCountry au
            oo = dropAlignedUnit (auo)
            subject = OrderSubject u pt
        in if country' == country
           then makeOrder country subject oo : ords
           else ords
orders country (AdjustBoard _ _ o) = M.foldrWithKey selectOrder [] o
  where
    selectOrder pt auo ords =
        let au = dropOrderObject auo
            u = alignedUnit au
            country' = alignedCountry au
            oo = dropAlignedUnit (auo)
            subject = OrderSubject u pt
        in if country' == country
           then makeOrder country subject oo : ords
           else ords

newtype AlignedUnitAndOrderObject phaseType
  = AlignedUnitAndOrderObject (Country, Unit, OrderObject phaseType)
    deriving (Show)

dropOrderObject :: AlignedUnitAndOrderObject phaseType -> AlignedUnit
dropOrderObject (AlignedUnitAndOrderObject (c, u, _)) = align u c

dropAlignedUnit :: AlignedUnitAndOrderObject phaseType -> OrderObject phaseType
dropAlignedUnit (AlignedUnitAndOrderObject (_, _, oo)) = oo

alignUnitOrder
  :: Country
  -> Unit
  -> OrderObject phaseType
  -> AlignedUnitAndOrderObject phaseType
alignUnitOrder c u o = AlignedUnitAndOrderObject (c, u, o)

-- | A report of which units were removed in normalization and from where.
type RemovedUnits = [(AlignedUnit, ProvinceTarget)]

-- | A value of this type serves as proof that an adjustment board is
--   normalized, meaning there are no supply centre to unit defecit.
--   The only way from an adjust board to a typical board is through this!
newtype NormalizedBoard = NormalizedBoard (Board Adjust, RemovedUnits)

endAdjustment :: Board Adjust -> (ResolvedOrders Adjust, Board Typical)
endAdjustment brd@(AdjustBoard o c ords) = (res, TypicalBoard o' c)
  where
    -- TODO must be careful to normalize only after we carry out the orders.
    nbrd = normalizeBoard brd
    -- TODO must build ResolvedOrders Adjust from RemovedUnits and the actual
    -- orders given.
    removed = removedUnits nbrd
    res = undefined
    brd' = normalizedBoard nbrd
    o' = undefined -- M.mapWithKey mapper brd'
    oo = defaultOrderObjectTypical
    mapper pt au = alignUnitOrder (alignedCountry au) (alignedUnit au) oo

-- | The board in the NormalizedBoard is guaranteed to have no supply centre
--   defecits, since the only way in to NormalizedBoard is via normalizeBoard
--   (unless you cheat and use the constructor directly, but you mustn't!).
--
--   Here we do exactly what we do for makeTypical, but with an AdjustBoard.
normalizedBoard :: NormalizedBoard -> Board Typical
normalizedBoard (NormalizedBoard (AdjustBoard o c _, _)) = TypicalBoard o' c
  where
    o' = M.mapWithKey mapper o
    oo = defaultOrderObjectTypical
    mapper pt au = alignUnitOrder (alignedCountry au) (alignedUnit au) oo

removedUnits :: NormalizedBoard -> RemovedUnits
removedUnits (NormalizedBoard (_, r)) = r

-- | The only way to make a NormalizedBoard. It guarantees that every
--   NormalizedBoard has the property that for any Country c
--
--     unitCount c <= supplyCentreCount c
--
--   by "randomly" knocking off (unitCount c - supplyCentreCount c) units
--   belonging to c. It's not really random, it's just an accident of
--   the order on ProvinceTarget. TBD is this a big deal? The alternative would
--   demand bringing in a pseudorandom number generator or :O making this an
--   IO !
normalizeBoard :: Board Adjust -> NormalizedBoard
normalizeBoard brd = NormalizedBoard $ M.foldrWithKey update (brd, []) defecits
  where
    defecits = supplyCentreDefecits brd
    update country surplus (brd, removedUnits) =
      let (brd', removedUnits') = removeSomeUnits surplus country brd'
      in  (brd', removedUnits' ++ removedUnits)

-- | Remove some number of units belonging to some country, arbitrarily.
--   The order is not random, but also not at all obvious; it depends upon the
--   order instance of ProvinceTarget. Units on earlier/lesser ProvinceTargets
--   will be removed.
removeSomeUnits :: Int -> Country -> Board Adjust -> (Board Adjust, RemovedUnits)
removeSomeUnits defecit country brd@(AdjustBoard o c ords) = (AdjustBoard o' c ords, removedUnits)
  where
    (_, o', removedUnits) = M.foldrWithKey update (defecit, o, []) o
    update pt au (defecit, o, removedUnits) =
      if defecit > 0 && (alignedCountry au == country)
      then (defecit - 1, M.delete pt o, (au, pt) : removedUnits)
      else (defecit, o, removedUnits)

supplyCentreCounts :: Board Adjust -> M.Map Country Int
supplyCentreCounts (AdjustBoard o c ords) = M.foldrWithKey updateCount M.empty c
  where
    updateCount pr country map =
      if supplyCentre pr
      then M.alter (maybe (Just 1) (Just . ((+) 1))) country map
      else map

-- | Number of supply centres countrolled by a given power.
supplyCentreCount :: Country -> Board a -> Int
supplyCentreCount c b = M.foldrWithKey count 0 (_controlled b)
  where count pr control t =
          if supplyCentre pr
          then if control == c then t + 1 else t
          else t

unitCounts :: Board Adjust -> M.Map Country Int
unitCounts (AdjustBoard o c ords) = M.foldrWithKey updateCount M.empty o
  where
    updateCount pt au map =
      M.alter (maybe (Just 1) (Just . ((+) 1))) (alignedCountry au) map

mapZipWith :: Ord k => (a -> Maybe b -> c) -> M.Map k a -> M.Map k b -> M.Map k c
mapZipWith f m1 m2 = M.foldrWithKey doZip M.empty m1
  where
    doZip key val = M.insert key (f val (M.lookup key m2))

-- | Values give the defecit for each country. That is, unit count minus
--   supply centre count. Possibly negative, in which case we have a surplus.
supplyCentreDefecits :: Board Adjust -> M.Map Country Int
supplyCentreDefecits brd = mapZipWith combine (unitCounts brd) (supplyCentreCounts brd)
  where
    combine unitCount Nothing = unitCount
    -- ^ Second argument is nothing means this country holds no supply centres.
    combine unitCount (Just supplyCentreCount) = unitCount - supplyCentreCount

supplyCentreSurplusses :: Board Adjust -> M.Map Country Int
supplyCentreSurplusses = M.map (\x -> -x) . supplyCentreDefecits

-- | Make a typical board become a retreat board. That's easy, just drop the
--   order object from the occupy map and give an empty dislodged map
--
--   TODO perhaps we should demand the order resolutions here so we can fill
--   in the dislodged map?
--   No, we should just produce the resolutions here and return them.
makeRetreat :: Board Typical -> Board Retreat
makeRetreat (TypicalBoard o c) = RetreatBoard (M.map dropOrderObject o) c M.empty res
  where
    -- TODO will compute this by resolving orders.
    res = undefined

-- | Making an adjustment board entails updating the control map so that every
--   occupying unit takes control of the province it occupies.
makeAdjust :: Board Retreat -> Board Adjust
makeAdjust (RetreatBoard o c d _) = AdjustBoard o c' M.empty
  where
    c' = M.foldrWithKey update c o
    update pt au c = M.insert (ptProvince pt) (alignedCountry au) c

-- | To make a Typical board from an Adjust phase, look elsewhere. You must
--   go through NormalizedBoard.
--   TODO this must depend upon previous order resolutions.
--   But then, why not just keep the previous order resolutions on the
--   retreat board??? Is this doing too much work?
--   No, that will be necessary, so that we can identify invalid withdraw
--   orders! So this type signature stands; we'll knock the previous resolutions
--   off of the Diplomacy type.
makeTypical :: Board Retreat -> Board Typical
makeTypical (RetreatBoard o c d _) = TypicalBoard o' c
  where
    o' = M.mapWithKey mapper o
    oo = defaultOrderObjectTypical
    mapper pt au = alignUnitOrder (alignedCountry au) (alignedUnit au) oo

_controlled :: Board a -> M.Map Province Country
_controlled (TypicalBoard _ c) = c
_controlled (RetreatBoard _ c _ _) = c
_controlled (AdjustBoard _ c _) = c

_dislodged :: Board Retreat -> M.Map ProvinceTarget (AlignedUnitAndOrderObject Retreat)
_dislodged (RetreatBoard _ _ d _) = d

-- | Low-level unsafe update of the board's control map.
updateBoardControl :: M.Map Province Country -> Board a -> Board a
updateBoardControl map (TypicalBoard o _) = TypicalBoard o map
updateBoardControl map (RetreatBoard o _ d res) = RetreatBoard o map d res
updateBoardControl map (AdjustBoard o _ ords) = AdjustBoard o map ords


updateBoardDislodge
  :: M.Map ProvinceTarget (AlignedUnitAndOrderObject Retreat)
  -> Board Retreat
  -> Board Retreat
updateBoardDislodge d (RetreatBoard o c _ res) = RetreatBoard o c d res

emptyBoard :: Board Typical
emptyBoard = TypicalBoard M.empty M.empty

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

occupy :: ProvinceTarget -> Maybe AlignedUnit -> Board Typical -> Board Typical
occupy pt maybeUnit (TypicalBoard o c) = TypicalBoard o' c
  where
    o' = M.alter (const nextValue) pt o
    oo = defaultOrderObjectTypical
    nextValue = fmap makeAlignedUnitOrder maybeUnit
    makeAlignedUnitOrder au = alignUnitOrder (alignedCountry au) (alignedUnit au) oo

control :: Province -> Maybe Country -> Board a -> Board a
control prv maybeCountry board = updateBoardControl newControl board
  where
    newControl = M.alter (const maybeCountry) prv (_controlled board)

dislodge :: ProvinceTarget -> Maybe AlignedUnit -> Board Retreat -> Board Retreat
dislodge pt maybeUnit brd = updateBoardDislodge newDislodge brd
  where
    oo = defaultOrderObjectRetreat
    newDislodge = M.alter (const nextValue) pt (_dislodged brd)
    nextValue = fmap makeAlignedUnitOrder maybeUnit
    makeAlignedUnitOrder au = alignUnitOrder (alignedCountry au) (alignedUnit au) oo

unitAt :: ProvinceTarget -> Board a -> Maybe AlignedUnit
unitAt pt (TypicalBoard o _) = fmap dropOrderObject (M.lookup pt o)
unitAt pt (RetreatBoard o _ _ _) = M.lookup pt o
unitAt pt (AdjustBoard o _ _) = M.lookup pt o

unitCount :: Country -> Board a -> Int
unitCount c (TypicalBoard o _) = M.foldr count 0 o
  where
    count auo t = if alignedCountry (dropOrderObject auo) == c
                  then t + 1
                  else t
unitCount c (RetreatBoard o _ d _) = occupyCount + dislodgedCount 
  where
    occupyCount = M.foldr countOccupy 0 o
    dislodgedCount = M.foldr countDislodged 0 d
    countOccupy unit t = if alignedCountry unit == c then t + 1 else t
    countDislodged auo t = if alignedCountry (dropOrderObject auo) == c
                           then t + 1
                           else t
unitCount c (AdjustBoard o _ _) = M.foldr count 0 o
  where
    count unit t = if alignedCountry unit == c then t + 1 else t

occupies :: AlignedUnit -> ProvinceTarget -> Board a -> Bool
occupies au pt (TypicalBoard o _) = maybe False check (M.lookup pt o)
  where
    check auo = dropOrderObject auo == au
occupies au pt (RetreatBoard o _ _ _) = maybe False ((==) au) (M.lookup pt o)
occupies au pt (AdjustBoard o _ _) = maybe False ((==) au) (M.lookup pt o)

controllerOf :: Board a -> Province -> Maybe Country
controllerOf board prv = M.lookup prv (_controlled board)

controls :: Board a -> Country -> Province -> Bool
controls b c p = maybe False ((==) c) (controllerOf b p)

-- | Get the dislodged unit, if any, at a given ProvinceTarget.
dislodged :: Board Retreat -> ProvinceTarget -> Maybe AlignedUnit
dislodged brd pt = fmap dropOrderObject (M.lookup pt (_dislodged brd))
