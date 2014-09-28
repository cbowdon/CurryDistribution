data Board = Board { turn :: Int
                   , chain :: [Player]
                   } deriving Show

type History = [Board]

data Player = Player { inventory :: Int
                     , isHuman :: Bool
                     , position :: Position
                     , orders :: [Int]
                     } deriving Show

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
    b' <- runTurn [b]
    print b'

runTurn :: History -> IO History
runTurn h = do
    co <- getCustomerOrder
    pi <- getPlayerOrder
    case pi of
        QuitGame -> return h
        Order pi -> runTurn $  updateBoard h co pi : h

generateBoard :: IO Board
generateBoard = undefined

getCustomerOrder :: IO Int
getCustomerOrder = undefined

getPlayerOrder :: IO PlayerInput
getPlayerOrder = undefined

-- customer to factory
subtractOrders :: [Player] -> [Player]
subtractOrders (p1:p2:ps) = subtractOrder p1 p2 : subtractOrders (p2:ps)
subtractOrders (p:_)      = [p]

-- Subtract p1's order from 2 turns ago from p2
subtractOrder :: Player -> Player -> Player
subtractOrder = moveProduct (-)

type AddOp = Int -> Int -> Int

moveProduct :: AddOp -> Player -> Player -> Player
moveProduct op p1 p2 = case orders p1 of
    o:_ -> p2 { inventory = inventory p2 `op` o }
    _   -> p2

dropOldestOrder :: Player -> Player
dropOldestOrder p = p { orders = drop 1 $ orders p }

updateBoard :: History -> Int -> Int -> Board
updateBoard = undefined
