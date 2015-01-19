{-# LANGUAGE GADTs #-}

module Diplomacy.Board (

    Board

  -- This group of exports are the only means through which Board values
  -- can come about.
  , initialBoard
  -- Functions for "advancing" a board from phase to phase.
  , makeRetreat
  , makeAdjust
  , makeTypical
  , endAdjustment
  , giveOrder

  , orders
  , occupation

  , DiplomacyMap
  , diplomacyMap

  , unitAt
  , controllerOf
  , getDislodged
  , hasDislodged
  , isDislodged
  , alignedUnitOccupies
  , controls
  , countryOccupies
  , unitOccupies 

  , supplyCentreCount

  , supplyCentreDefecits

  , previousResolvedOrders

  ) where

import qualified Data.Map as M
import           Control.Applicative
import           Data.Monoid
import qualified Data.Foldable as F

import           Diplomacy.Phase (Typical, Retreat, Adjust)
import           Diplomacy.Country
import           Diplomacy.Province
import           Diplomacy.Unit
import           Diplomacy.PlayerCount
import           Diplomacy.Order
import           Diplomacy.ResolvedOrder
import           Diplomacy.Occupation
import           Diplomacy.Dislodgement
import           Diplomacy.Defecits

-- | TODO explain. We don't put AlignedUnits on the board, we put
--   BoardEntry values, which contain a Unit and Country anyways.
type BoardEntry phaseType = SomeValidOrder phaseType

-- | Values of type Board phaseType capture the state of a diplomacy game at
--   a phase of that type (we use a phase type rather than a phase because
--   there is no difference in game state form between Spring/Autumn and
--   SpringRetreat/AutumnRetreat). This includes: occupation of province
--   targets, control of countries, orders for each occupying unit in a typical
--   phase, each dislodged unit and its order in a retreat phase, and a set of
--   adjustment orders in an adjustment phase.
data Board phaseType where

  TypicalBoard
    :: Occupation (BoardEntry Typical)
    -- ^ Occupation of ProvinceTargets.
    -> M.Map Province Country
    -- ^ Control of provinces.
    -> Board Typical

  RetreatBoard
    :: Occupation AlignedUnit
    -- ^ Occupation of province targets. Note that on a retreat
    -- board there is no associated order. Instead, we put the
    -- orders on the dislodged units.
    -> M.Map Province Country
    -- ^ Control of provinces.
    -> Dislodgement (BoardEntry Retreat)
    -- ^ Dislodged units.
    -> ResolvedOrders Typical
    -- ^ Resolved orders from the previous phase; they are necessary to
    -- determine validity of retreat phase orders.
    -> Board Retreat

  AdjustBoard
    :: Occupation AlignedUnit
    -- ^ Occupation of province targets. There is no associated
    -- order, because in the adjustment phase, no units require
    -- orders. We handle unit vs. supply centre count in the
    -- NormalizedBoard datatype.
    -> M.Map Province Country
    -- ^ Control of provinces.
    -> M.Map ProvinceTarget (BoardEntry Adjust)
    -- ^ Track the adjustment orders. We do not group it with either of the
    -- other two maps because orders are not required at Adjust phase.
    -> Board Adjust

-- | A DiplomacyMap is a Board without any orders. The only way to create one
--   is from a Board. It contains the information about a board which every
--   player is allowed to know.
data DiplomacyMap phaseType where

  TypicalMap
    :: Occupation AlignedUnit
    -> M.Map Province Country
    -> DiplomacyMap Typical

  RetreatMap
    :: Occupation AlignedUnit
    -> M.Map Province Country
    -> Dislodgement AlignedUnit
    -> DiplomacyMap Retreat

  AdjustMap
    :: Occupation AlignedUnit
    -> M.Map Province Country
    -> DiplomacyMap Adjust

diplomacyMap :: Board phaseType -> DiplomacyMap phaseType
diplomacyMap (TypicalBoard o c) = TypicalMap (fmap (consumeSomeValidOrder validOrderToAlignedUnit) o) c
diplomacyMap (RetreatBoard o c d _) = RetreatMap o c (fmap (consumeSomeValidOrder validOrderToAlignedUnit) d)
diplomacyMap (AdjustBoard o c _) = AdjustMap o c

validateOrder
  :: Order phaseType orderType
  -> Board phaseType
  -> OrderValidation phaseType orderType
validateOrder order board = case board of
   TypicalBoard o _ -> validateOrderTypical (fmap (consumeSomeValidOrder validOrderToAlignedUnit) o) order 
   RetreatBoard _ _ d r -> validateOrderRetreat (fmap (consumeSomeValidOrder validOrderToAlignedUnit) d) r order
   AdjustBoard o _ _ -> validateOrderAdjust o order

-- | Give an order. The board is unchanged in case the order is invalid.
giveOrder
  :: Order phaseType orderType
  -> Board phaseType
  -> (OrderValidation phaseType orderType, Board phaseType)
giveOrder order board = (validation, nextBoard)
  where
    validation = validateOrder order board
    nextBoard = case validation of
      -- Note the safe use of giveValidOrder; it comes from validateOrder
      -- against the same board.
      Right validOrder -> giveValidOrder validOrder board
      leftInvalidOrder -> board

-- | ValidOrder must have been generated by validateOrder against this very
--   board!
giveValidOrder :: ValidOrder phaseType orderType -> Board phaseType -> Board phaseType
giveValidOrder validOrder (TypicalBoard occupyMap controlMap) =
    TypicalBoard nextOccupyMap controlMap
  where
    nextOccupyMap = insertOccupation provinceTarget (makeSomeValidOrder validOrder) occupyMap
    provinceTarget = validOrderToSubjectTarget validOrder
giveValidOrder validOrder (RetreatBoard x y dislodgedMap z) =
    RetreatBoard x y nextDislodgedMap z
  where
    nextDislodgedMap = insertDislodgement provinceTarget (makeSomeValidOrder validOrder) dislodgedMap
    provinceTarget = validOrderToSubjectTarget validOrder
giveValidOrder validOrder (AdjustBoard x y orderMap) =
    AdjustBoard x y nextOrderMap
  where
    nextOrderMap = M.alter alteration provinceTarget orderMap
    provinceTarget = validOrderToSubjectTarget validOrder
    alteration = const $ Just (makeSomeValidOrder validOrder)

validOrders :: Board phaseType -> [SomeValidOrder phaseType]
validOrders (TypicalBoard o _) = F.foldr (:) [] o
validOrders (RetreatBoard _ _ d _) = F.foldr (:) [] d
validOrders (AdjustBoard _ _ o) = F.foldr (:) [] o

orders :: Board phaseType -> ValidOrders phaseType
orders brd@(TypicalBoard _ _) = makeValidOrdersTypical (validOrders brd)
orders brd@(RetreatBoard _ _ _ _) = makeValidOrdersRetreat (validOrders brd)
orders brd@(AdjustBoard _ _ _) = makeValidOrdersAdjust (validOrders brd)

-- | A report of which units were removed in normalization and from where.
type RemovedUnits = [(AlignedUnit, ProvinceTarget)]

-- | A value of this type serves as proof that an adjustment board is
--   normalized, meaning there are no supply centre to unit defecit.
--   The only way from an adjust board to a typical board is through this!
newtype NormalizedBoard = NormalizedBoard (Board Adjust, RemovedUnits)

endAdjustment :: Board Adjust -> (ResolvedOrders Adjust, Board Typical)
endAdjustment = undefined
{-
TODO
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
-}
-- | The board in the NormalizedBoard is guaranteed to have no supply centre
--   defecits, since the only way in to NormalizedBoard is via normalizeBoard
--   (unless you cheat and use the constructor directly, but you mustn't!).
--
--   Here we do exactly what we do for makeTypical, but with an AdjustBoard.
--
--   TODO FIXME TODO
normalizedBoard :: NormalizedBoard -> Board Typical
normalizedBoard (NormalizedBoard (AdjustBoard o c _, _)) = TypicalBoard undefined undefined

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
    o' = undefined
    removedUnits = undefined
    {-
    (_, o', removedUnits) = M.foldrWithKey update (defecit, o, []) o
    update pt au (defecit, o, removedUnits) =
      if defecit > 0 && (alignedCountry au == country)
      then (defecit - 1, M.delete pt o, (au, pt) : removedUnits)
      else (defecit, o, removedUnits)
    -}

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
unitCounts (AdjustBoard o c ords) = F.foldr updateCount M.empty o
  where
    updateCount au map =
      M.alter (maybe (Just 1) (Just . ((+) 1))) (alignedCountry au) map

mapZipWith :: Ord k => (a -> Maybe b -> c) -> M.Map k a -> M.Map k b -> M.Map k c
mapZipWith f m1 m2 = M.foldrWithKey doZip M.empty m1
  where
    doZip key val = M.insert key (f val (M.lookup key m2))

-- | Values give the defecit for each country. That is, unit count minus
--   supply centre count. Possibly negative, in which case we have a surplus.
supplyCentreDefecits :: Board Adjust -> Defecits
supplyCentreDefecits brd = mapZipWith combine (unitCounts brd) (supplyCentreCounts brd)
  where
    combine unitCount Nothing = unitCount
    -- ^ Second argument is nothing means this country holds no supply centres.
    combine unitCount (Just supplyCentreCount) = unitCount - supplyCentreCount

-- | Make a typical board become a retreat board. That's easy, just drop the
--   order object from the occupy map and give an empty dislodged map
--
--   TODO perhaps we should demand the order resolutions here so we can fill
--   in the dislodged map?
--   No, we should just produce the resolutions here and return them.
makeRetreat :: Board Typical -> (ResolvedOrders Typical, Board Retreat)
makeRetreat board = (resolutions, nextBoard)
  where
    resolutions = undefined -- resolveOrders (validOrders board)
    nextBoard = undefined -- TODO applyResolutionsTypical resolutions board

-- | Making an adjustment board entails updating the control map so that every
--   occupying unit takes control of the province it occupies.
makeAdjust :: Board Retreat -> (ResolvedOrders Retreat, Board Adjust)
makeAdjust board = (resolutions, nextBoard)
  where
    resolutions = undefined -- resolveOrders (validOrders board)
    nextBoard = undefined -- TODO applyResolutionsRetreatAdjust resolutions board

-- | To make a Typical board from an Adjust phase, look elsewhere. You must
--   go through NormalizedBoard.
--   TODO this must depend upon previous order resolutions.
--   But then, why not just keep the previous order resolutions on the
--   retreat board??? Is this doing too much work?
--   No, that will be necessary, so that we can identify invalid withdraw
--   orders! So this type signature stands; we'll knock the previous resolutions
--   off of the Diplomacy type.
makeTypical :: Board Retreat -> (ResolvedOrders Retreat, Board Typical)
makeTypical board = (resolutions, nextBoard)
  where
    resolutions = undefined -- resolveOrders (validOrders board)
    nextBoard = undefined -- TODO applyResolutionsRetreatTypical resolutions board

_controlled :: Board a -> M.Map Province Country
_controlled (TypicalBoard _ c) = c
_controlled (RetreatBoard _ c _ _) = c
_controlled (AdjustBoard _ c _) = c

_dislodged :: Board Retreat -> Dislodgement (SomeValidOrder Retreat)
_dislodged (RetreatBoard _ _ d _) = d

-- | Low-level unsafe update of the board's control map.
updateBoardControl :: M.Map Province Country -> Board a -> Board a
updateBoardControl map (TypicalBoard o _) = TypicalBoard o map
updateBoardControl map (RetreatBoard o _ d res) = RetreatBoard o map d res
updateBoardControl map (AdjustBoard o _ ords) = AdjustBoard o map ords

{-
TODO
updateBoardDislodge
  :: M.Map ProvinceTarget (AlignedUnitAndOrderObject Retreat)
  -> Board Retreat
  -> Board Retreat
updateBoardDislodge d (RetreatBoard o c _ res) = RetreatBoard o c d res
-}

emptyBoard :: Board Typical
emptyBoard = TypicalBoard mempty M.empty

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
occupy provinceTarget maybeUnit board = case maybeUnit of
    Nothing -> removeOccupy provinceTarget board
    Just au -> insertOccupy provinceTarget (alignedCountry au) (alignedUnit au) board

removeOccupy :: ProvinceTarget -> Board Typical -> Board Typical
removeOccupy provinceTarget (TypicalBoard o c) = TypicalBoard o' c
  where
    o' = removeOccupation provinceTarget o

insertOccupy :: ProvinceTarget -> Country -> Unit -> Board Typical -> Board Typical
insertOccupy provinceTarget country unit (TypicalBoard o c) = TypicalBoard o' c
  where
    o' = insertOccupation provinceTarget (makeSomeValidOrder defaultValidOrder) o
    defaultValidOrder = validOrderTypical country provinceTarget unit

control :: Province -> Maybe Country -> Board a -> Board a
control prv maybeCountry board = updateBoardControl newControl board
  where
    newControl = M.alter (const maybeCountry) prv (_controlled board)

dislodge :: ProvinceTarget -> Maybe AlignedUnit -> Board Retreat -> Board Retreat
dislodge provinceTarget maybeUnit board = case maybeUnit of
    Nothing -> removeDislodge provinceTarget board
    Just au -> insertDislodge provinceTarget (alignedCountry au) (alignedUnit au) board

removeDislodge :: ProvinceTarget -> Board Retreat -> Board Retreat
removeDislodge provinceTarget (RetreatBoard x y d z) = RetreatBoard x y d' z
  where
    d' = removeDislodgement provinceTarget d

insertDislodge :: ProvinceTarget -> Country -> Unit -> Board Retreat -> Board Retreat
insertDislodge provinceTarget country unit (RetreatBoard x y d z) = RetreatBoard x y d' z
  where
    d' = insertDislodgement provinceTarget (makeSomeValidOrder defaultValidOrder) d
    defaultValidOrder = validOrderRetreat country provinceTarget unit

unitAt :: ProvinceTarget -> Board phaseType -> Maybe AlignedUnit
unitAt pt (TypicalBoard o _) = fmap (consumeSomeValidOrder validOrderToAlignedUnit) (checkOccupation pt o)
unitAt pt (RetreatBoard o _ _ _) = checkOccupation pt o
unitAt pt (AdjustBoard o _ _) = checkOccupation pt o

alignedUnitOccupies :: AlignedUnit -> ProvinceTarget -> Board phaseType -> Bool
alignedUnitOccupies au pt board = case board of
    TypicalBoard o _ -> maybe False check (checkOccupation pt o)
      where
        check = ((==) au) . (consumeSomeValidOrder validOrderToAlignedUnit)
    RetreatBoard o _ _ _ -> maybe False ((==) au) (checkOccupation pt o)
    AdjustBoard o _ _ -> maybe False ((==) au) (checkOccupation pt o)

controllerOf :: Board a -> Province -> Maybe Country
controllerOf board prv = M.lookup prv (_controlled board)

controls :: Board a -> Country -> Province -> Bool
controls b c p = maybe False ((==) c) (controllerOf b p)

-- | Get the dislodged unit, if any, at a given ProvinceTarget.
getDislodged :: Board Retreat -> ProvinceTarget -> Maybe AlignedUnit
getDislodged brd pt = fmap (consumeSomeValidOrder validOrderToAlignedUnit) (checkDislodgement pt (_dislodged brd))

hasDislodged :: Board Retreat -> ProvinceTarget -> Bool
hasDislodged board = maybe False (const True) . getDislodged board

isDislodged :: Board Retreat -> AlignedUnit -> ProvinceTarget -> Bool
isDislodged board au pt = maybe False ((==) au) (getDislodged board pt)

countryOccupies :: Country -> ProvinceTarget -> Board a -> Bool
countryOccupies country pt (TypicalBoard o _) = maybe False check (checkOccupation pt o)
  where
    check = ((==) country) . (alignedCountry . (consumeSomeValidOrder validOrderToAlignedUnit))
countryOccupies country pt (RetreatBoard o _ _ _) = maybe False check (checkOccupation pt o)
  where
    check au = alignedCountry au == country
countryOccupies country pt (AdjustBoard o _ _) = maybe False check (checkOccupation pt o)
  where
    check au = alignedCountry au == country

unitOccupies :: Unit -> ProvinceTarget -> Board a -> Bool
unitOccupies unit pt (TypicalBoard o _) = maybe False check (checkOccupation pt o)
  where
    check = ((==) unit) . (alignedUnit . (consumeSomeValidOrder validOrderToAlignedUnit))
unitOccupies unit pt (RetreatBoard o _ _ _) = maybe False check (checkOccupation pt o)
  where
    check au = alignedUnit au == unit
unitOccupies unit pt (AdjustBoard o _ _) = maybe False check (checkOccupation pt o)
  where
    check au = alignedUnit au == unit

previousResolvedOrders
  :: Board Retreat
  -> ResolvedOrders Typical
previousResolvedOrders (RetreatBoard _ _ _ rs) = rs

occupation :: Board phaseType -> Occupation AlignedUnit
occupation (TypicalBoard occ _) = fmap (consumeSomeValidOrder validOrderToAlignedUnit) occ
occupation (RetreatBoard occ _ _ _) = occ
occupation (AdjustBoard occ _ _) = occ
