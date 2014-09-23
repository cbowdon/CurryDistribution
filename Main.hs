{-
 - We're going to approach this with type-first programming. That is, we use the type system to design our program.
 - One of the best features of Haskell is 'undefined'. It's a dummy definition that will throw a runtime error.
 - Sounds useless, but is actually very useful; it allows you to check the types compile without implementing anything.
 -}
main :: IO ()
main = undefined

{-
 - It's a turn-based game, so there will be a board that is updated on every input from the player.
 - There will be a data structure to contain the state of the board.
 - The data we care about is the turn number and the state of the supply chain, which is a list of players.
 -}
data Board = Board { turn :: Int
                   , chain :: [Player]
                   } deriving Show

{-
 - There will also be a function that updates the board on each turn.
 - Its inputs will be the current board, the Customer's order and the player's input (their order).
 -}
updateBoard :: Board -> Int -> Int -> Board
updateBoard = undefined

{-
 - The player has an associated inventory and backlog.
 - They have a position in the supply chain.
 - They may not be human.
 -}
data Player = Player { backlog :: Int
                     , inventory :: Int
                     , isHuman :: Bool
                     , position :: Position
                     } deriving Show

{-
 - Position may not actually matter since every player in the chain has the same behaviour except those at the ends.
 - We may even want to experiment with chains of arbitrary length. Perhaps we will redefine this later.
 -}
data Position = Customer | Retailer | Wholesaler | Distributor | Factory

{-
 - There will be a function to update a player with an imminent arrival and order.
 - It will deduct from inventory and add to backlog as appropriate.
 -}
updatePlayer :: Player -> Int -> Int -> Player
updatePlayer :: undefined

{-
 - The challenging part is representing the time-delayed orders.
 - Remember, upstream players only receive the order after 1 turn, and the downstream player only receives the product after 2.
 - Perhaps a better way to model the entire game would be as a sequence of events updating the board.
 - The current state of the board can be derived by applying the events. This is known as "event sourcing".
 -}
