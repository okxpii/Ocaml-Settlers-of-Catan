(** Creates and maintains the board. *)

open Player

type tile = {
  tile_id : int;
  resource : resource;
  dice_num : int;
  has_robber : bool;
}
(** [tile] represents the tile and its resources and game settings *)

type node
(** [node] represents the node where a players can setle *)

type edge
(** [edge] represents the roads between nodes *)

type settlement
(** [settlement] represents player that settled a node *)

type city
(** [city] represents an updated settlement *)

type road
(** [road] represents the edge where owned by a player*)

type port
(** [port] represents the ports players can use *)

type board
(** [board] represents nodes, edges, and players *)


val tile_list : tile list
(** [tile_list] is the list of tiles ad the start of the game. *)

val node_list : node list
(** [node_list] is the list of nodes ad the start of the game. *)

val edge_list : edge list
(** [edge_list] is the list of edges ad the start of the game. *)

val string_resource : resource -> string
(** [string_resource] returns the string that is associated with a
    resource [res]. Requires: [res] must be of type resource. *)

val string_of_port : port -> string
(** [string_of_port] returns the string that is associated with a port
    [p]. Requires: [p] must be of type port. *)

val string_of_tile : tile -> string
(** [string_of_tile] returns the string that is associated with a tile
    [t]. Requires: [t] must be of type tile. *)

val string_of_adj_node : node -> string
(** [string_of_adj_node] returns the string that is associated with an
    adjacent node [n]. Requires: [n] must be of type node. *)

val adj_nodes_to_string : int list -> string
(** [adj_nodes_to_string] returns a string version of adjacent nodes,
    which is in int form [l]. Requires: [l] must be of type int list. *)

val adj_edges_to_string : int list -> string
(** [adj_edges_to_string] returns a string version of adjacent edges,
    which is in int form [l]. Requires: [l] must be of type int list. *)

val adj_tiles_to_string : int list -> string
(** [adj_tiles_to_string] returns a string version of adjacent tiles,
    which is in int form [l]. Requires: [l] must be of type int list. *)

val string_of_edge : edge -> string
(** [string_of_edge] returns the string version of an edge [e].
    Requires: [e] must be of type edge. *)

val string_of_node : node -> string
(** [string_of_node] returns the string version of a node [n]. Requires:
    [n] must be of type node. *)

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

val build_road : int -> player -> edge list -> edge list
(** [build_road ind player board] returns the list of edges including the
    modified edge. Requires: [ind] is an integer in the range 0-71 and
    [player] is of type player, and [board] is the current list of edges *)

val build_settlement : int -> player -> node list -> node list
(** [build_settlement ind player board] returns the list of nodes including the
    modified node. Requires: [ind] is an integer in the range 0-53 and
    [player] is of type player, and [board] is the current list of nodes. *)

val get_resource : int -> (int * resource) list
(** [get_resource ind] returns the list of tiles correspodning to that
    number on the dice roll Requires: [ind] is an integer in the range
    2-12. *)

val draw_board : tile list -> node list -> edge list -> unit
(** [get_resource tiles nodes edges] is the visual aid for users to use
    when playing the game. It displays the game in a hexaognal pattern,
    with nodes, edges and tiles. When a user populates a node or edge,
    it is colored to their user color. This function takes the list
    nodes, edges, and tiles and outputs the display (view in the source code): 

                                        <27>---33--<16>
                                        /             \
                                       39             23
                                      /                 \
                        <38>---49--<28>    D6 Ore 7     <17>---18--<07>
                        /             \                 /             \
                       54             40               24             10
                      /                 \             /                 \
        <47>---62--<39>   D3 Brick 12   <29>---34--<18>   D4 Brick 3    <08>---06--<00>
        /             \                 /             \                 /             \
       66             55               41             25               11             00
      /                 \             /                 \             /                 \
   <48>  D11 Wheat 16   <40>---50--<30>   D5 Sheep 8    <19>---19--<09>   D6 Wood 0     <01>
      \                 /             \                 /             \                 /
      67               56             42               26             12               01
        \             /                 \             /                 \             /
        <49>---63--<41>   D2 Wood 13    <31>---35--<20>   D3 Sheep 4    <10>---07--<02>
        /             \                 /             \                 /             \
       68             57               43             27               13             02
      /                 \             /                 \             /                 \
   <50>   D4 Wood 17    <42>---51--<32>     Desert 9    <21>---20--<11>   D5 Wheat 1    <03>
      \                 /             \                 /             \                 /
      69               58             44               28             14               03
        \             /                 \             /                 \             /
        <51>---64--<43>  D10 Wheat 14   <33>---36--<22>   D8 Brick 5    <12>---08--<04>
        /             \                 /             \                 /             \
       70             59               45             29               15             04
      /                 \             /                 \             /                 \
   <52>   D7 Sheep 18   <44>---52--<34>   D9 Wheat 10   <23>---21--<13>    D9 Ore 2     <05>
      \                 /             \                 /             \                 /
      71               60             46               30             16               05
        \             /                 \             /                 \             /
        <53>---65--<45>  D11 Wood 15    <35>---37--<24>   D10 Ore 6     <14>---09--<06>
                      \                 /             \                 /
                      61               47             31               17
                        \             /                 \             /
                        <46>---53--<36>  D12 Sheep 11   <25>---22--<15>
                                      \                 /
                                      48               32
                                        \             /
                                        <37>---38--<26>
    *)
