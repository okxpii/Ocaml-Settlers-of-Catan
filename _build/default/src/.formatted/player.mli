type resource =
  | Wheat
  | Sheep
  | Brick
  | Wood
  | Ore
  | Desert

type color =
  | Red
  | Blue
  | Yellow
  | Green
  | Brown
  | White

type development_card

type player = {
  player_color : color;
  resources : resource list;
  development_cards : development_card list;
  score : int;
  num_settlements : int;
  num_cities : int;
  num_roads : int;
  mutable has_rolled : bool;
  mutable played_card : bool;
}

val player_test : player
val string_color : color -> string
val node_color : player -> color
val string_of_resource : resource -> string
val string_of_card : development_card -> string
val string_of_cards : development_card list -> string
val string_of_resources : resource list -> string
val resource_of_string : string -> resource
val card_of_string : string -> development_card
val string_of_player : player -> string
val node_color : player -> string
