(** Parses user entries into commands for the game  *)

exception Empty
(** [Empty] is to catch for when users input empty strings *)

exception Invalid
(** [Invalid] is to catch for when users input invalid commands *)


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

  (** [command] represents the possible actions the game can take *)

val parse_string : string -> command
(** [parse_string] parses a player's string input into a command. It
    converts all strings to lowercase to assist with usability *)
