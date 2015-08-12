import qualified Data.Map as M
import Diplomacy.Unit
import Diplomacy.Province
import Diplomacy.Zone
import Diplomacy.OrderObject
import Diplomacy.GreatPower
import Diplomacy.Aligned
import Diplomacy.Game

-- Orders for first Typical Phase (first round).
firstOrders = M.fromList [
      (Zone (Normal London), (align Fleet England, SomeOrderObject (MoveObject (Normal EnglishChannel))))
    , (Zone (Normal Edinburgh), (align Fleet England, SomeOrderObject (MoveObject (Normal NorthSea))))
    , (Zone (Special StPetersburgSouth), (align Fleet Russia, SomeOrderObject (MoveObject (Normal GulfOfBothnia))))
    , (Zone (Normal Marseilles), (align Army France, SomeOrderObject (MoveObject (Normal Piedmont))))
    ]

-- Orders for second Typical phase (third round).
secondOrders = M.fromList [
      (Zone (Normal Piedmont), (align Army France, SomeOrderObject (MoveObject (Normal Venice))))
    , (Zone (Normal Trieste), (align Fleet Austria, SomeOrderObject (SupportObject (Army, Normal Piedmont) (Normal Venice))))
    ]

-- Orders for second Retreat phase (fourth round).
thirdOrders = M.fromList [
      (Zone (Normal Venice), (align Army Italy, SomeOrderObject (WithdrawObject (Normal Tuscany))))
    ]

main = do
    let game0 = snd (issueOrders firstOrders newGame)
    putStrLn (showGame game0)
    putStrLn "\nPress any key to see next typical phase.\n"
    getChar
    let game1 = snd (issueOrders secondOrders (continue . resolve . continue . resolve $ game0))
    putStrLn (showGame game1)
    putStrLn "\nPress any key to see the next retreat phase."
    putStrLn "Note that the Italian army in Venice is dislodged and retreats to Tuscany.\n"
    getChar
    let game2 = snd (issueOrders thirdOrders (continue . resolve $ game1))
    putStrLn (showGame game2)
