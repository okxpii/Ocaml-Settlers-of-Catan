(** The player, resources, and associated types used in the game 
    for player properties  *)

type resource =
  | Wheat
  | Sheep
  | Brick
  | Wood
  | Ore
  | Desert

(** [resource] is the possible resources a player can recieve and use  *)

type color =
  | Red
  | Blue
  | Yellow
  | Green
  | Brown
  | White

(** [color] is the possible player colors  *)

type development_card
(** [development_card] is a card a player can earn and use  *)

type player = {
  player_color : color;
  resources : resource list;
  development_cards : development_card list;
  score : int;
  num_settlements : int;
  num_cities : int;
  num_roads : int;
  mutable has_rolled : bool;
}
(** [player] is the player type  *)

val player_test : player
(** [player_test] is a practice player for testing *)

val string_color : color -> string
(** [string_color] returns the string version of a color. Requires:
    input must be of type color. *)

val node_color : player -> string
(** [node_color] returns the string version of a [player] player's
    color. Requires: [player] must be of valid player. *)

val string_of_resource : resource -> string
(** [string_of_resource] returns the string version of a resource.
    Requires: input must be of type resource. *)

val string_of_card : development_card -> string
(** [string_of_card] returns the string version of a card [res].
    Requires: [res] must be of type development_card. *)

val string_of_cards : development_card list -> string
(** [string_of_cards] returns the string version of a development_card
    list [l]. Requires: [l] must be of type development_card list. *)

val string_of_resources : resource list -> string
(** [string_of_resources] returns the string version of a resource list
    [l]. Requires: [l] must be of type resource list. *)

val resource_of_string : string -> resource
(** [resource_of_string] returns the resource that is associated with a
    certain string [str]. Requires: [str] must be of type string. *)

val card_of_string : string -> development_card
(** [card_of_string] returns the development_card that is associated
    with a certain string [str]. Requires: [str] must be of type string. *)

val string_of_player : player -> string
(** [string_of_player] returns the string version of a certain player
    [p]. Requires: [p] must be of type player. *)
