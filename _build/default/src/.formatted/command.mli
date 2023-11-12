exception Empty
exception Invalid

type command =
  | Start
  | Quit
  | Roll
  | Empty
  | Invalid
  | Settle
  | BuildRoad
  | PlayCard
  | EndTurn
  | CheckResources
  | CheckSettlements
  | CheckRoads
  | CheckScore
  | CheckCards
  | BuyCard

val parse_string : string -> command
(** [parse_string] parses a player's string input into a command. It
    converts all strings to lowercase to assist with usability *)
