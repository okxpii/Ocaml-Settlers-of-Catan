open Player

type tile = {
  tile_id : int;
  resource : resource;
  dice_num : int;
  has_robber : bool;
}

type node
type edge
type settlement
type city
type road
type port
type intersection
type board

val tile_list : tile list
val node_list : node list
val edge_list : edge list
val test_resource : string -> resource
val init_tile : int -> resource -> int -> tile
val init_node : int -> int list -> int list -> int list -> node
val init_edge : int -> edge
val string_resource : resource -> string
val string_of_edge : edge -> string
val string_of_node : node -> string

val get_tile : int -> tile list -> tile
(** [get_tile ind tiles] returns the tile at the index [ind] in the list
    of tiles. Requires: [ind] is an integer in the range 0-18 and
    [tiles] is a list of type t. *)

val get_node : int -> node list -> node
(** [get_node ind nodes] returns the node at the index [ind] in the list
    of nodes. Requires: [ind] is an integer in the range 0-53 and
    [nodes] is a list of type n. *)

val get_edge : int -> edge list -> edge
(** [get_edge ind edges] returns the node at the index [ind] in the list
    of edges. Requires: [ind] is an integer in the range 0-71 and
    [edges] is a list of type e. *)

val build_road : int -> Player.player -> edge list -> edge list
(** [build_road ind edge] returns the list of edges including the
    modified edge. Requires: [ind] is an integer in the range 0-71 and
    [edge] is of type e *)

val build_settlement : int -> Player.player -> node list -> node list
(** [build_settlement ind node] returns the list of nodes including the
    modified node. Requires: [ind] is an integer in the range 0-53 and
    [node] is of type n. *)

val get_resource : int -> (int * resource) list
(** [get_resource ind] returns the list of tiles correspodning to that
    number on the dice roll Requires: [ind] is an integer in the range
    2-12. *)

val draw_board : tile list -> node list -> edge list -> unit
(** [get_resource tiles nodes edges] is the visual aid for users to use
    when playing the game. It displays the game in a hexaognal pattern,
    with nodes, edges and tiles. When a user populates a node or edge,
    it is colored to their user color. This function takes the list
    nodes, edges, and tiles and outputs the display: <27>---33--<16> / \
    39 23 / \ <38>---49--<28> D6 Ore 7 <17>---18--<07> / \ / \ 54 40 24
    10 / \ / \ <47>---62--<39> D3 Brick 12 <29>---34--<18> D4 Brick 3
    <08>---06--<00> / \ / \ / \ 66 55 41 25 11 00 / \ / \ / \ <48> D11
    Wheat 16 <40>---50--<30> D5 Sheep 8 <19>---19--<09> D6 Wood 0 <01> \
    / \ / \ / 67 56 42 26 12 01 \ / \ / \ / <49>---63--<41> D2 Wood 13
    <31>---35--<20> D3 Sheep 4 <10>---07--<02> / \ / \ / \ 68 57 43 27
    13 02 / \ / \ / \ <50> D4 Wood 17 <42>---51--<32> Desert 9
    <21>---20--<11> D5 Wheat 1 <03> \ / \ / \ / 69 58 44 28 14 03 \ / \
    / \ / <51>---64--<43> D10 Wheat 14 <33>---36--<22> D8 Brick 5
    <12>---08--<04> / \ / \ / \ 70 59 45 29 15 04 / \ / \ / \ <52> D8
    Sheep 18 <44>---52--<34> D9 Wheat 10 <23>---21--<13> D9 Ore 2 <05> \
    / \ / \ / 71 60 46 30 16 05 \ / \ / \ / <53>---65--<45> D11 Wood 15
    <35>---37--<24> D10 Ore 6 <14>---09--<06> \ / \ / 61 47 31 17 \ / \
    / <46>---53--<36> D12 Sheep 11 <25>---22--<15> \ / 48 32 \ /
    <37>---38--<26> *)
