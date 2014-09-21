# Curry Distribution

This is the Beer Distribution Game, implemented in Haskell.

## Rules (gleaned from the net)

### Objective
To provide curry as demanded, at minimal cost. There are 2 sources of cost:
* holding inventory
* having outstanding orders to fulfill (backlog)
In other words, you must provide precisely the right amount of curry or be penalized.

### Players (the supply chain)
There are four potentially-human roles:

* Factory
* Distributor
* Wholesaler
* Retailer

These form a supply chain, with a non-human player, the Customer, downstream of the retailer.

There is no direct communication between players.

### Turns
Each turn covers a week. The actual size of the unit doesn't matter, it just represents a realistic delay in communications and shipping.

On each turn:
1. Players receive the product they ordered 2 turns ago from the upstream player.
2. Players receive the order placed 1 turn ago by the downstream player and have this amount deducted from their inventory/added to their backlog.
3. Players decide an amount to order with their upstream player. (The Customer will place pseudo-random orders.)

A reasonable maximum turns is something like 52 (i.e. a year of business). The aim is to simulate ongoing business, so it may be desirable to _lie about_ or _not reveal_ the maximum number of turns, to encourage players to avoid a short-term strategy.

### Scoring

This may well be tweaked a bit later.

Most importantly:
* Inventory carrying cost: $1/korma
* Backlog cost: $2/korma

This aims to represent how not fulfilling orders is more damaging to a business than having a surplus (though neither is good). Additionally you can add cost and income to the placing and receiving of orders itself:

* Purchase cost: $1/korma
* Sales income: $4/korma

It might work to have an initial bank balance and to end the game after any part of the supply chain is bankrupted (or max turns is reached).
