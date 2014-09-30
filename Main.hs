import Data.List (intersperse)

data Board = Board { turn :: Int
                   , chain :: [Player]
                   }

instance Show Board where
    show b = let t = show $ turn b
                 c = foldr (++) "" . intersperse "\n" . map show $ chain b
             in t ++ replicate 10 '-' ++ "\n" ++ c

type History = [Board]

data Player = Player { inventory :: Int
                     , isHuman :: Bool
                     , position :: Position
                     , orders :: [Int]
                     }

instance Show Player where
    show p = foldr (++) "" . intersperse " " $ [ show (position p)
                                               , show (inventory p)
                                               , show (orders p)
                                               , if isHuman p then "(human)" else "(AI)"]

data Position = Customer |
                Retailer |
                Wholesaler |
                Distributor |
                Factory |
                Robots deriving Show

data PlayerInput = Order Int | QuitGame deriving Show

initialOrders = [0, 0]

initialPlayers = [ Player { inventory = 100
                          , isHuman = False
                          , position = Factory
                          , orders = initialOrders }
                 , Player { inventory = 100
                          , isHuman = False
                          , position = Distributor
                          , orders = initialOrders }
                 , Player { inventory = 100
                          , isHuman = False
                          , position = Distributor
                          , orders = initialOrders }
                 , Player { inventory = 100
                          , isHuman = True
                          , position = Wholesaler
                          , orders = initialOrders }
                 , Player { inventory = 100
                          , isHuman = False
                          , position = Retailer
                          , orders = initialOrders }
                 , Player { inventory = 100
                          , isHuman = False
                          , position = Customer
                          , orders = initialOrders } ]

initialBoard = Board { turn = 0, chain = initialPlayers }

main :: IO ()
main = do
    b <- generateBoard
    (b':_) <- runTurn [b]
    print b'

runTurn :: History -> IO History
runTurn hist@(b:_) = do
    print b
    co <- getCustomerOrder
    pi <- getPlayerOrder
    case pi of
        QuitGame -> return hist
        Order pi -> runTurn $ updateBoard b co pi : hist

generateBoard :: IO Board
generateBoard = return initialBoard -- TODO randomize

getCustomerOrder :: IO Int
getCustomerOrder = return 10 -- TODO randomize

getPlayerOrder :: IO PlayerInput
getPlayerOrder = fmap parseInput getLine
    where
        parseInput i
            | i == "q"  = QuitGame
            | otherwise = Order . read $ i

updateBoard :: Board -> Int -> Int -> Board
updateBoard b co po = b { turn = turn b + 1
                        , chain = recordOrders co po . applyUpdates $ chain b }

recordOrders :: Int -> Int -> [Player] -> [Player]
recordOrders co po = map recordOrder
    where
        recordOrder p = case position p of
            Customer -> p { orders = co:orders p }
            _        -> if isHuman p
                        then p { orders = po:orders p }
                        else p { orders = 10:orders p } -- TODO AI

applyUpdates :: [Player] -> [Player]
applyUpdates =
    map dropOldestOrder .
    reverse . -- TODO reverse reverse...
    subtractOrders .
    reverse .
    addShipments

subtractOrders :: [Player] -> [Player]
subtractOrders = moveProducts subtractOrder

addShipments :: [Player] -> [Player]
addShipments = moveProducts addShipment

moveProducts :: (Player -> Player -> Player) -> [Player] -> [Player]
moveProducts f (p1:p2:ps) = f p1 p2 : moveProducts f (p2:ps)
moveProducts f (p:_) = [p]

-- Subtract p1's order from 2 turns ago from p2
subtractOrder :: Player -> Player -> Player
subtractOrder = moveProduct (-)

addShipment :: Player -> Player -> Player
addShipment = flip $ moveProduct (+)

type AddOp = Int -> Int -> Int

moveProduct :: AddOp -> Player -> Player -> Player
moveProduct op p1 p2 = case orders p1 of
    o:_ -> p2 { inventory = inventory p2 `op` o }
    _   -> p2

dropOldestOrder :: Player -> Player
dropOldestOrder p = p { orders = drop 1 $ orders p }
