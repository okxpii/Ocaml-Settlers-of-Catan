# Settlers of Caml-tan

Kayla Riggs (kmr242)
Javier Vega (jdv67)
Xicheng Lin (xl377)
Mohammad Hamzah (mh2296)

**Game Rules:**

**Players:** Red & Blue

**Objective:** The first player to receive 10 Victory Points wins the game 

**Set-Up:**
At the start of every turn, a player should roll the dice. The dice roll corresponds to one or two tiles on the board; resources from those tiles will be added to the player’s resource list. To roll, a player should type **roll**. Typically, Catan is played where a player can build as many settlements and roads and buy as many development cards as they wish per turn, but they can only play one development card per turn. Players should do their best to adhere to this rule throughout the duration of the game. 

**Construction:** 

**Settling:**
- A player must have a wood, brick, sheep, and wheat in their resources to settle. When they’ve successfully settled, one of each of these resources will be removed from their resources. 
- The node on the board at which they settle will turn to the corresponding color (either red or blue) after a player has settled.
- A player cannot build a settlement in the same place that another player already built a settlement. 
- Settling adds one Victory Point to the player’s score.
- To settle, a player should type **settle**.

**Building a Road:**
- A player must have a wood and brick in their resources to build a road. When they’ve successfully built a road, one of each of these resources will be removed from their resources. 
- The edge on the board at which they settle will turn to the corresponding color (either red or blue) after a player has settled. 
- A player cannot build a road in the same place that another player already built a road. 
- To build a road, a player should type **build road**.


**Development Cards:**

A player must have a sheep, wheat, and ore in their resources to buy a development card. Once they buy a development card, it will appear in their development_card list; a Victory Point will also be added to their score. Additionally, once a development card is bought, one of each of the above resources will be removed from their resources. To buy a card, a player should type buy card; to play a card, a player should type **play card**.
The cards that can be purchased or played are as follows: monopoly, year of plenty, road building, and victory point.  Once a card is played, it will disappear from their development_card list. 

*NOTE: A player must type in the correct command (monopoly, year of plenty, victory point, road building) when prompted which card they’d like to play or buy*

**Monopoly:**
To use this card, the player must enter the name of the resource they want to take in the terminal. Once they do so, any of that resource that the other players possess will be added to the current player's collection of resources. To play a monopoly card, a player should follow the prompts and type **monopoly** after **play card**.

**Year of Plenty:**
A player may select two resources of their choice from the bank when they play this card. They will be added to their resources. To play a year of plenty card, a player should follow the prompts and type **year of plenty** after **play card**.

**Victory Point:**
A player is awarded one victory point when they play this card. To play a victory point card, a player should follow the prompts and type **victory point** after **play card**. 

**Road Building:**
A player may build two roads in one turn when playing this card. To play a road building card, a player should follow the prompts and type **road building** after **play card.**

**Other Commands:**

**quit**: This will end the entire program (and game)

**check cards**: A current player can check the cards in their development_cards list with this command.

**check resources**: A current player can check the resources in their resources list with this command.

**check score**: A current player can check their score with this command.

**check settlements**: A current player can check the number of settlements they have with this command.

**check roads**:  A current player can check the number of roads they have with this command.

**end turn**:  A current player can end their turn with this command. The program automatically moves to the next player’s turn. 
