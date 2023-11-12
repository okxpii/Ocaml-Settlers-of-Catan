open Player
open Random

type settlement = { owner : player }
type city = { owner : player }
type intersection = { owner : player option }

type port = {
  port_resource : resource;
  ratio : int;
}

type edge = {
  edge_id : int;
  has_road : bool;
  owner : player option;
}

type tile = {
  tile_id : int;
  resource : resource;
  dice_num : int;
  has_robber : bool;
}

type node = {
  node_id : int;
  adj_nodes : int list;
  adj_edges : int list;
  adj_tiles : int list;
  port : port;
  has_settlement : bool;
  owner : player option;
}

type road = {
  road_id : int;
  start_node : node;
  end_node : node;
  mutable is_connected : bool;
  mutable owner : player option;
}

type board = {
  nodes : node list;
  mutable roads : road list;
  players : player list;
}
(* type board = { nodes : node list; edges : edge list; tiles : tile
   list; intersections : intersection list; mutable roads : road list;
   mutable cities : cities list; mutable ports : ports list; mutable
   settlements : settlements list; players : player list; } *)

let string_resource res =
  match res with
  | Wheat -> "Wheat"
  | Desert -> "Desert"
  | Sheep -> "Sheep"
  | Brick -> "Brick"
  | Wood -> "Wood"
  | Ore -> "Ore"

let string_of_port p =
  Printf.sprintf "{port_resource=%s; ratio=%d;}"
    (string_resource p.port_resource)
    p.ratio

let string_of_tile t =
  Printf.sprintf
    "{tile_id=%d; resource=%s; dice_num=%d; has_robber=%b; \n}"
    t.tile_id
    (string_resource t.resource)
    t.dice_num t.has_robber

let init_tile i res dice =
  { tile_id = i; resource = res; dice_num = dice; has_robber = false }

let string_of_edge e =
  Printf.sprintf "{edge_id=%d; has_road=%b; owner=%s; \n}" e.edge_id
    e.has_road
    (match e.owner with
    | Some i -> string_of_player i
    | None -> "no owner")

let init_edge i = { edge_id = i; has_road = false; owner = None }

let string_of_adj_node n =
  Printf.sprintf "{node_id=%d; port=%s; has_settlement=%b; owner=%s;}"
    n.node_id "d" n.has_settlement
    (match n.owner with
    | Some i -> string_of_player i
    | None -> "no owner")

let adj_nodes_to_string l =
  String.concat "; " (List.map string_of_int l)

let adj_edges_to_string l =
  String.concat "; " (List.map string_of_int l)

let adj_tiles_to_string l =
  String.concat "; " (List.map string_of_int l)

let string_of_node n =
  Printf.sprintf
    "{node_id=%d; adj_nodes=%s; adj_edges=%s; adj_tiles=%s; port=%s; \
     has_settlement=%b; owner=%s; \n\
     }"
    n.node_id
    (adj_nodes_to_string n.adj_nodes)
    (adj_edges_to_string n.adj_edges)
    (adj_tiles_to_string n.adj_tiles)
    (string_of_port n.port) n.has_settlement
    (match n.owner with
    | Some i -> string_of_player i
    | None -> "no owner")

let init_node i node_list edge_list tile_list =
  {
    node_id = i;
    adj_nodes = node_list;
    adj_edges = edge_list;
    adj_tiles = tile_list;
    port = { port_resource = Desert; ratio = 3 };
    has_settlement = false;
    owner = None;
  }

let tile_list =
  [
    init_tile 0 Wood 6;
    init_tile 1 Wheat 5;
    init_tile 2 Ore 9;
    init_tile 3 Brick 4;
    init_tile 4 Sheep 3;
    init_tile 5 Brick 8;
    init_tile 6 Ore 10;
    init_tile 7 Ore 6;
    init_tile 8 Sheep 5;
    init_tile 9 Desert 0;
    init_tile 10 Wheat 9;
    init_tile 11 Sheep 12;
    init_tile 12 Brick 3;
    init_tile 13 Wood 2;
    init_tile 14 Wheat 10;
    init_tile 15 Wood 11;
    init_tile 16 Wheat 11;
    init_tile 17 Wood 4;
    init_tile 18 Sheep 7;
  ]

let edge_list =
  [
    init_edge 0;
    init_edge 1;
    init_edge 2;
    init_edge 3;
    init_edge 4;
    init_edge 5;
    init_edge 6;
    init_edge 7;
    init_edge 8;
    init_edge 9;
    init_edge 10;
    init_edge 11;
    init_edge 12;
    init_edge 13;
    init_edge 14;
    init_edge 15;
    init_edge 16;
    init_edge 17;
    init_edge 18;
    init_edge 19;
    init_edge 20;
    init_edge 21;
    init_edge 22;
    init_edge 23;
    init_edge 24;
    init_edge 25;
    init_edge 26;
    init_edge 27;
    init_edge 28;
    init_edge 29;
    init_edge 30;
    init_edge 31;
    init_edge 32;
    init_edge 33;
    init_edge 34;
    init_edge 35;
    init_edge 36;
    init_edge 37;
    init_edge 38;
    init_edge 39;
    init_edge 40;
    init_edge 41;
    init_edge 42;
    init_edge 43;
    init_edge 44;
    init_edge 45;
    init_edge 46;
    init_edge 47;
    init_edge 48;
    init_edge 49;
    init_edge 50;
    init_edge 51;
    init_edge 52;
    init_edge 53;
    init_edge 54;
    init_edge 55;
    init_edge 56;
    init_edge 57;
    init_edge 58;
    init_edge 59;
    init_edge 60;
    init_edge 61;
    init_edge 62;
    init_edge 63;
    init_edge 64;
    init_edge 65;
    init_edge 66;
    init_edge 67;
    init_edge 68;
    init_edge 69;
    init_edge 70;
    init_edge 71;
  ]

let node_list =
  [
    init_node 0 [ 1; 8 ] [ 0; 6 ] [ 0 ];
    init_node 1 [ 0; 2 ] [ 0; 1 ] [ 0 ];
    init_node 2 [ 1; 3; 10 ] [ 1; 2; 7 ] [ 0; 1 ];
    init_node 3 [ 2; 4 ] [ 2; 3 ] [ 1 ];
    init_node 4 [ 3; 5; 12 ] [ 3; 4; 8 ] [ 1; 2 ];
    init_node 5 [ 4; 6 ] [ 4; 5 ] [ 2 ];
    init_node 6 [ 5; 14 ] [ 5; 9 ] [ 2 ];
    init_node 7 [ 8; 17 ] [ 10; 18 ] [ 3 ];
    init_node 8 [ 0; 7; 9 ] [ 6; 10; 11 ] [ 0; 3 ];
    init_node 9 [ 8; 10; 19 ] [ 11; 12; 19 ] [ 0; 3; 4 ];
    init_node 10 [ 2; 9; 11 ] [ 7; 12; 13 ] [ 0; 1; 4 ];
    init_node 11 [ 10; 12; 21 ] [ 13; 14; 20 ] [ 1; 4; 5 ];
    init_node 12 [ 4; 11; 13 ] [ 8; 14; 15 ] [ 1; 2; 5 ];
    init_node 13 [ 12; 14; 23 ] [ 15; 16; 21 ] [ 2; 5; 6 ];
    init_node 14 [ 6; 13; 15 ] [ 9; 16; 17 ] [ 2; 6 ];
    init_node 15 [ 14; 25 ] [ 17; 22 ] [ 6 ];
    init_node 16 [ 17; 27 ] [ 23; 33 ] [ 7 ];
    init_node 17 [ 7; 16; 18 ] [ 18; 23; 24 ] [ 3; 7 ];
    init_node 18 [ 17; 19; 29 ] [ 24; 25; 34 ] [ 3; 7; 8 ];
    init_node 19 [ 9; 18; 20 ] [ 19; 25; 26 ] [ 3; 4; 8 ];
    init_node 20 [ 19; 21; 31 ] [ 26; 27; 35 ] [ 4; 8; 9 ];
    init_node 21 [ 11; 20; 22 ] [ 20; 27; 28 ] [ 4; 5; 9 ];
    init_node 22 [ 21; 23; 33 ] [ 28; 29; 36 ] [ 5; 9; 10 ];
    init_node 23 [ 13; 22; 24 ] [ 21; 29; 30 ] [ 5; 6; 10 ];
    init_node 24 [ 23; 25; 35 ] [ 30; 31; 37 ] [ 6; 10; 11 ];
    init_node 25 [ 15; 24; 26 ] [ 22; 31; 32 ] [ 6; 11 ];
    init_node 26 [ 25; 37 ] [ 32; 38 ] [ 11 ];
    init_node 27 [ 16; 28 ] [ 33; 39 ] [ 7 ];
    init_node 28 [ 27; 29; 38 ] [ 39; 40; 49 ] [ 7; 12 ];
    init_node 29 [ 18; 28; 30 ] [ 34; 40; 41 ] [ 7; 8; 12 ];
    init_node 30 [ 29; 31; 40 ] [ 41; 42; 50 ] [ 8; 12; 13 ];
    init_node 31 [ 20; 30; 32 ] [ 35; 42; 43 ] [ 8; 9; 13 ];
    init_node 32 [ 31; 33; 42 ] [ 43; 44; 51 ] [ 9; 13; 14 ];
    init_node 33 [ 22; 32; 34 ] [ 36; 44; 45 ] [ 9; 10; 14 ];
    init_node 34 [ 33; 35; 44 ] [ 45; 46; 52 ] [ 10; 14; 15 ];
    init_node 35 [ 24; 34; 36 ] [ 37; 46; 47 ] [ 10; 11; 15 ];
    init_node 36 [ 35; 37; 46 ] [ 47; 48; 53 ] [ 11; 15 ];
    init_node 37 [ 26; 36 ] [ 38; 48 ] [ 11 ];
    init_node 38 [ 28; 39 ] [ 49; 54 ] [ 12 ];
    init_node 39 [ 38; 40; 47 ] [ 54; 55; 62 ] [ 12; 16 ];
    init_node 40 [ 30; 39; 41 ] [ 50; 55; 56 ] [ 12; 13; 16 ];
    init_node 41 [ 40; 42; 49 ] [ 56; 57; 63 ] [ 13; 16; 17 ];
    init_node 42 [ 32; 41; 43 ] [ 51; 57; 58 ] [ 13; 14; 17 ];
    init_node 43 [ 42; 44; 51 ] [ 58; 59; 64 ] [ 14; 17; 18 ];
    init_node 44 [ 34; 43; 45 ] [ 52; 59; 60 ] [ 14; 15; 18 ];
    init_node 45 [ 44; 46; 53 ] [ 60; 61; 65 ] [ 15; 18 ];
    init_node 46 [ 36; 45 ] [ 53; 61 ] [ 15 ];
    init_node 47 [ 39; 48 ] [ 62; 66 ] [ 16 ];
    init_node 48 [ 47; 49 ] [ 66; 67 ] [ 16 ];
    init_node 49 [ 41; 48; 50 ] [ 63; 67; 68 ] [ 16; 17 ];
    init_node 50 [ 49; 51 ] [ 68; 69 ] [ 17 ];
    init_node 51 [ 43; 50; 52 ] [ 64; 69; 70 ] [ 17; 18 ];
    init_node 52 [ 51; 53 ] [ 70; 71 ] [ 18 ];
    init_node 53 [ 45; 52 ] [ 65; 71 ] [ 18 ];
  ]

let get_tile ind tiles = List.find (fun t -> t.tile_id = ind) tiles
let get_node ind nodes = List.find (fun n -> n.node_id = ind) nodes
let get_edge ind edges = List.find (fun e -> e.edge_id = ind) edges

let build_road ind player board =
  List.map
    (fun e ->
      if e.edge_id = ind && e.owner = None then
        { edge_id = e.edge_id; has_road = true; owner = Some player }
      else get_edge e.edge_id board)
    board

let build_settlement ind player board =
  List.map
    (fun n ->
      if n.node_id = ind && n.owner = None then
        {
          node_id = n.node_id;
          adj_nodes = n.adj_nodes;
          adj_edges = n.adj_edges;
          adj_tiles = n.adj_tiles;
          port = n.port;
          has_settlement = true;
          owner = Some player;
        }
      else get_node n.node_id board)
    board

let get_resource ind =
  tile_list
  |> List.filter (fun dice -> dice.dice_num = ind)
  |> List.map (fun res -> (res.tile_id, res.resource))

(* The color of player pl as an ANSITERMINAL.color *)
let get_color (pl : player option) =
  match pl with
  | None -> ANSITerminal.White
  | Some p -> (
      match p.player_color with
      | Red -> ANSITerminal.Red
      | Blue -> ANSITerminal.Blue
      | Yellow -> ANSITerminal.Yellow
      | Green -> ANSITerminal.Green
      | White -> ANSITerminal.White
      | _ -> failwith "Color not supported by ANSITERMINAL")

let draw_tile id text tile_lst =
  let t = get_tile id tile_lst in
  ANSITerminal.print_string [] text

let draw_node id text node_lst =
  let n = get_node id node_lst in
  let color = get_color n.owner in
  ANSITerminal.print_string
    [ ANSITerminal.Foreground color ]
    ("<" ^ text ^ ">")

let draw_edge id text edge_lst =
  let e = get_edge id edge_lst in
  let color = get_color e.owner in
  ANSITerminal.print_string [ ANSITerminal.Foreground color ] text

let draw_board tiles nodes edges =
  ANSITerminal.print_string [] "\n";
  ANSITerminal.print_string []
    "                                        ";
  draw_node 27 "27" nodes;
  draw_edge 33 "---33--" edges;
  draw_node 16 "16" nodes;
  ANSITerminal.print_string [] "\n";
  ANSITerminal.print_string []
    "                                        ";
  draw_edge 39 "/" edges;
  ANSITerminal.print_string [] "             ";
  draw_edge 23 "\\" edges;
  ANSITerminal.print_string [] "\n";
  ANSITerminal.print_string [] "                                       ";
  draw_edge 39 "39" edges;
  ANSITerminal.print_string [] "             ";
  draw_edge 23 "23" edges;
  ANSITerminal.print_string [] "\n";
  ANSITerminal.print_string [] "                                      ";
  draw_edge 39 "/" edges;
  ANSITerminal.print_string [] "                 ";
  draw_edge 23 "\\" edges;
  ANSITerminal.print_string [] "\n";
  ANSITerminal.print_string [] "                        ";
  draw_node 38 "38" nodes;
  draw_edge 49 "---49--" edges;
  draw_node 28 "28" nodes;
  draw_tile 7 "    D6 Ore 7     " tiles;
  draw_node 17 "17" nodes;
  draw_edge 18 "---18--" edges;
  draw_node 7 "07" nodes;
  ANSITerminal.print_string [] "\n";
  ANSITerminal.print_string [] "                        ";
  draw_edge 54 "/" edges;
  ANSITerminal.print_string [] "             ";
  draw_edge 40 "\\" edges;
  ANSITerminal.print_string [] "                 ";
  draw_edge 24 "/" edges;
  ANSITerminal.print_string [] "             ";
  draw_edge 10 "\\" edges;
  ANSITerminal.print_string [] "\n";
  ANSITerminal.print_string [] "                       ";
  draw_edge 54 "54" edges;
  ANSITerminal.print_string [] "             ";
  draw_edge 40 "40" edges;
  ANSITerminal.print_string [] "               ";
  draw_edge 24 "24" edges;
  ANSITerminal.print_string [] "             ";
  draw_edge 10 "10" edges;
  ANSITerminal.print_string [] "\n";
  ANSITerminal.print_string [] "                      ";
  draw_edge 54 "/" edges;
  ANSITerminal.print_string [] "                 ";
  draw_edge 40 "\\" edges;
  ANSITerminal.print_string [] "             ";
  draw_edge 24 "/" edges;
  ANSITerminal.print_string [] "                 ";
  draw_edge 10 "\\" edges;
  ANSITerminal.print_string [] "\n";
  ANSITerminal.print_string [] "        ";
  draw_node 47 "47" nodes;
  draw_edge 62 "---62--" edges;
  draw_node 39 "39" nodes;
  draw_tile 12 "   D3 Brick 12   " tiles;
  draw_node 29 "29" nodes;
  draw_edge 34 "---34--" edges;
  draw_node 18 "18" nodes;
  draw_tile 3 "   D4 Brick 3    " tiles;
  draw_node 8 "08" nodes;
  draw_edge 6 "---06--" edges;
  draw_node 0 "00" nodes;
  ANSITerminal.print_string [] "\n";
  ANSITerminal.print_string [] "        ";
  draw_edge 66 "/" edges;
  ANSITerminal.print_string [] "             ";
  draw_edge 55 "\\" edges;
  ANSITerminal.print_string [] "                 ";
  draw_edge 41 "/" edges;
  ANSITerminal.print_string [] "             ";
  draw_edge 25 "\\" edges;
  ANSITerminal.print_string [] "                 ";
  draw_edge 11 "/" edges;
  ANSITerminal.print_string [] "             ";
  draw_edge 0 "\\" edges;
  ANSITerminal.print_string [] "\n";
  ANSITerminal.print_string [] "       ";
  draw_edge 66 "66" edges;
  ANSITerminal.print_string [] "             ";
  draw_edge 55 "55" edges;
  ANSITerminal.print_string [] "               ";
  draw_edge 41 "41" edges;
  ANSITerminal.print_string [] "             ";
  draw_edge 25 "25" edges;
  ANSITerminal.print_string [] "               ";
  draw_edge 11 "11" edges;
  ANSITerminal.print_string [] "             ";
  draw_edge 0 "00" edges;
  ANSITerminal.print_string [] "\n";
  ANSITerminal.print_string [] "      ";
  draw_edge 66 "/" edges;
  ANSITerminal.print_string [] "                 ";
  draw_edge 55 "\\" edges;
  ANSITerminal.print_string [] "             ";
  draw_edge 41 "/" edges;
  ANSITerminal.print_string [] "                 ";
  draw_edge 25 "\\" edges;
  ANSITerminal.print_string [] "             ";
  draw_edge 11 "/" edges;
  ANSITerminal.print_string [] "                 ";
  draw_edge 0 "\\" edges;
  ANSITerminal.print_string [] "\n";
  ANSITerminal.print_string [] "   ";
  draw_node 48 "48" nodes;
  draw_tile 16 "  D11 Wheat 16   " tiles;
  draw_node 40 "40" nodes;
  draw_edge 50 "---50--" edges;
  draw_node 30 "30" nodes;
  draw_tile 8 "   D5 Sheep 8    " tiles;
  draw_node 19 "19" nodes;
  draw_edge 19 "---19--" edges;
  draw_node 9 "09" nodes;
  draw_tile 0 "   D6 Wood 0     " tiles;
  draw_node 1 "01" nodes;
  ANSITerminal.print_string [] "\n";
  ANSITerminal.print_string [] "      ";
  draw_edge 67 "\\" edges;
  ANSITerminal.print_string [] "                 ";
  draw_edge 56 "/" edges;
  ANSITerminal.print_string [] "             ";
  draw_edge 42 "\\" edges;
  ANSITerminal.print_string [] "                 ";
  draw_edge 26 "/" edges;
  ANSITerminal.print_string [] "             ";
  draw_edge 12 "\\" edges;
  ANSITerminal.print_string [] "                 ";
  draw_edge 1 "/" edges;
  ANSITerminal.print_string [] "\n";
  ANSITerminal.print_string [] "      ";
  draw_edge 67 "67" edges;
  ANSITerminal.print_string [] "               ";
  draw_edge 56 "56" edges;
  ANSITerminal.print_string [] "             ";
  draw_edge 42 "42" edges;
  ANSITerminal.print_string [] "               ";
  draw_edge 26 "26" edges;
  ANSITerminal.print_string [] "             ";
  draw_edge 12 "12" edges;
  ANSITerminal.print_string [] "               ";
  draw_edge 1 "01" edges;
  ANSITerminal.print_string [] "\n";
  ANSITerminal.print_string [] "        ";
  draw_edge 67 "\\" edges;
  ANSITerminal.print_string [] "             ";
  draw_edge 56 "/" edges;
  ANSITerminal.print_string [] "                 ";
  draw_edge 42 "\\" edges;
  ANSITerminal.print_string [] "             ";
  draw_edge 26 "/" edges;
  ANSITerminal.print_string [] "                 ";
  draw_edge 12 "\\" edges;
  ANSITerminal.print_string [] "             ";
  draw_edge 1 "/" edges;
  ANSITerminal.print_string [] "\n";
  ANSITerminal.print_string [] "        ";
  draw_node 49 "49" nodes;
  draw_edge 63 "---63--" edges;
  draw_node 41 "41" nodes;
  draw_tile 13 "   D2 Wood 13    " tiles;
  draw_node 31 "31" nodes;
  draw_edge 35 "---35--" edges;
  draw_node 20 "20" nodes;
  draw_tile 4 "   D3 Sheep 4    " tiles;
  draw_node 10 "10" nodes;
  draw_edge 7 "---07--" edges;
  draw_node 2 "02" nodes;
  ANSITerminal.print_string [] "\n";
  ANSITerminal.print_string [] "        ";
  draw_edge 68 "/" edges;
  ANSITerminal.print_string [] "             ";
  draw_edge 57 "\\" edges;
  ANSITerminal.print_string [] "                 ";
  draw_edge 43 "/" edges;
  ANSITerminal.print_string [] "             ";
  draw_edge 27 "\\" edges;
  ANSITerminal.print_string [] "                 ";
  draw_edge 13 "/" edges;
  ANSITerminal.print_string [] "             ";
  draw_edge 2 "\\" edges;
  ANSITerminal.print_string [] "\n";
  ANSITerminal.print_string [] "       ";
  draw_edge 68 "68" edges;
  ANSITerminal.print_string [] "             ";
  draw_edge 57 "57" edges;
  ANSITerminal.print_string [] "               ";
  draw_edge 43 "43" edges;
  ANSITerminal.print_string [] "             ";
  draw_edge 27 "27" edges;
  ANSITerminal.print_string [] "               ";
  draw_edge 13 "13" edges;
  ANSITerminal.print_string [] "             ";
  draw_edge 2 "02" edges;
  ANSITerminal.print_string [] "\n";
  ANSITerminal.print_string [] "      ";
  draw_edge 68 "/" edges;
  ANSITerminal.print_string [] "                 ";
  draw_edge 57 "\\" edges;
  ANSITerminal.print_string [] "             ";
  draw_edge 43 "/" edges;
  ANSITerminal.print_string [] "                 ";
  draw_edge 27 "\\" edges;
  ANSITerminal.print_string [] "             ";
  draw_edge 13 "/" edges;
  ANSITerminal.print_string [] "                 ";
  draw_edge 2 "\\" edges;
  ANSITerminal.print_string [] "\n";
  ANSITerminal.print_string [] "   ";
  draw_node 50 "50" nodes;
  draw_tile 17 "   D4 Wood 17    " tiles;
  draw_node 42 "42" nodes;
  draw_edge 51 "---51--" edges;
  draw_node 32 "32" nodes;
  draw_tile 9 "     Desert 9    " tiles;
  draw_node 21 "21" nodes;
  draw_edge 20 "---20--" edges;
  draw_node 11 "11" nodes;
  draw_tile 1 "   D5 Wheat 1    " tiles;
  draw_node 3 "03" nodes;
  ANSITerminal.print_string [] "\n";
  ANSITerminal.print_string [] "      ";
  draw_edge 69 "\\" edges;
  ANSITerminal.print_string [] "                 ";
  draw_edge 58 "/" edges;
  ANSITerminal.print_string [] "             ";
  draw_edge 44 "\\" edges;
  ANSITerminal.print_string [] "                 ";
  draw_edge 28 "/" edges;
  ANSITerminal.print_string [] "             ";
  draw_edge 14 "\\" edges;
  ANSITerminal.print_string [] "                 ";
  draw_edge 3 "/" edges;
  ANSITerminal.print_string [] "\n";
  ANSITerminal.print_string [] "      ";
  draw_edge 69 "69" edges;
  ANSITerminal.print_string [] "               ";
  draw_edge 58 "58" edges;
  ANSITerminal.print_string [] "             ";
  draw_edge 44 "44" edges;
  ANSITerminal.print_string [] "               ";
  draw_edge 28 "28" edges;
  ANSITerminal.print_string [] "             ";
  draw_edge 14 "14" edges;
  ANSITerminal.print_string [] "               ";
  draw_edge 3 "03" edges;
  ANSITerminal.print_string [] "\n";
  ANSITerminal.print_string [] "        ";
  draw_edge 69 "\\" edges;
  ANSITerminal.print_string [] "             ";
  draw_edge 58 "/" edges;
  ANSITerminal.print_string [] "                 ";
  draw_edge 44 "\\" edges;
  ANSITerminal.print_string [] "             ";
  draw_edge 28 "/" edges;
  ANSITerminal.print_string [] "                 ";
  draw_edge 14 "\\" edges;
  ANSITerminal.print_string [] "             ";
  draw_edge 3 "/" edges;
  ANSITerminal.print_string [] "\n";
  ANSITerminal.print_string [] "        ";
  draw_node 51 "51" nodes;
  draw_edge 64 "---64--" edges;
  draw_node 43 "43" nodes;
  draw_tile 14 "  D10 Wheat 14   " tiles;
  draw_node 33 "33" nodes;
  draw_edge 36 "---36--" edges;
  draw_node 22 "22" nodes;
  draw_tile 5 "   D8 Brick 5    " tiles;
  draw_node 12 "12" nodes;
  draw_edge 8 "---08--" edges;
  draw_node 4 "04" nodes;
  ANSITerminal.print_string [] "\n";
  ANSITerminal.print_string [] "        ";
  draw_edge 70 "/" edges;
  ANSITerminal.print_string [] "             ";
  draw_edge 59 "\\" edges;
  ANSITerminal.print_string [] "                 ";
  draw_edge 45 "/" edges;
  ANSITerminal.print_string [] "             ";
  draw_edge 29 "\\" edges;
  ANSITerminal.print_string [] "                 ";
  draw_edge 15 "/" edges;
  ANSITerminal.print_string [] "             ";
  draw_edge 4 "\\" edges;
  ANSITerminal.print_string [] "\n";
  ANSITerminal.print_string [] "       ";
  draw_edge 70 "70" edges;
  ANSITerminal.print_string [] "             ";
  draw_edge 59 "59" edges;
  ANSITerminal.print_string [] "               ";
  draw_edge 45 "45" edges;
  ANSITerminal.print_string [] "             ";
  draw_edge 29 "29" edges;
  ANSITerminal.print_string [] "               ";
  draw_edge 15 "15" edges;
  ANSITerminal.print_string [] "             ";
  draw_edge 4 "04" edges;
  ANSITerminal.print_string [] "\n";
  ANSITerminal.print_string [] "      ";
  draw_edge 70 "/" edges;
  ANSITerminal.print_string [] "                 ";
  draw_edge 59 "\\" edges;
  ANSITerminal.print_string [] "             ";
  draw_edge 45 "/" edges;
  ANSITerminal.print_string [] "                 ";
  draw_edge 29 "\\" edges;
  ANSITerminal.print_string [] "             ";
  draw_edge 15 "/" edges;
  ANSITerminal.print_string [] "                 ";
  draw_edge 4 "\\" edges;
  ANSITerminal.print_string [] "\n";
  ANSITerminal.print_string [] "   ";
  draw_node 52 "52" nodes;
  draw_tile 18 "   D7 Sheep 18   " tiles;
  draw_node 44 "44" nodes;
  draw_edge 52 "---52--" edges;
  draw_node 34 "34" nodes;
  draw_tile 10 "   D9 Wheat 10   " tiles;
  draw_node 23 "23" nodes;
  draw_edge 21 "---21--" edges;
  draw_node 13 "13" nodes;
  draw_tile 2 "    D9 Ore 2     " tiles;
  draw_node 5 "05" nodes;
  ANSITerminal.print_string [] "\n";
  ANSITerminal.print_string [] "      ";
  draw_edge 71 "\\" edges;
  ANSITerminal.print_string [] "                 ";
  draw_edge 60 "/" edges;
  ANSITerminal.print_string [] "             ";
  draw_edge 46 "\\" edges;
  ANSITerminal.print_string [] "                 ";
  draw_edge 30 "/" edges;
  ANSITerminal.print_string [] "             ";
  draw_edge 16 "\\" edges;
  ANSITerminal.print_string [] "                 ";
  draw_edge 5 "/" edges;
  ANSITerminal.print_string [] "\n";
  ANSITerminal.print_string [] "      ";
  draw_edge 71 "71" edges;
  ANSITerminal.print_string [] "               ";
  draw_edge 60 "60" edges;
  ANSITerminal.print_string [] "             ";
  draw_edge 46 "46" edges;
  ANSITerminal.print_string [] "               ";
  draw_edge 30 "30" edges;
  ANSITerminal.print_string [] "             ";
  draw_edge 16 "16" edges;
  ANSITerminal.print_string [] "               ";
  draw_edge 5 "05" edges;
  ANSITerminal.print_string [] "\n";
  ANSITerminal.print_string [] "        ";
  draw_edge 71 "\\" edges;
  ANSITerminal.print_string [] "             ";
  draw_edge 60 "/" edges;
  ANSITerminal.print_string [] "                 ";
  draw_edge 46 "\\" edges;
  ANSITerminal.print_string [] "             ";
  draw_edge 30 "/" edges;
  ANSITerminal.print_string [] "                 ";
  draw_edge 16 "\\" edges;
  ANSITerminal.print_string [] "             ";
  draw_edge 5 "/" edges;
  ANSITerminal.print_string [] "\n";
  ANSITerminal.print_string [] "        ";
  draw_node 53 "53" nodes;
  draw_edge 65 "---65--" edges;
  draw_node 45 "45" nodes;
  draw_tile 15 "  D11 Wood 15    " tiles;
  draw_node 35 "35" nodes;
  draw_edge 37 "---37--" edges;
  draw_node 24 "24" nodes;
  draw_tile 6 "   D10 Ore 6     " tiles;
  draw_node 14 "14" nodes;
  draw_edge 9 "---09--" edges;
  draw_node 6 "06" nodes;
  ANSITerminal.print_string [] "\n";
  ANSITerminal.print_string [] "                      ";
  draw_edge 61 "\\" edges;
  ANSITerminal.print_string [] "                 ";
  draw_edge 47 "/" edges;
  ANSITerminal.print_string [] "             ";
  draw_edge 31 "\\" edges;
  ANSITerminal.print_string [] "                 ";
  draw_edge 17 "/" edges;
  ANSITerminal.print_string [] "\n";
  ANSITerminal.print_string [] "                      ";
  draw_edge 61 "61" edges;
  ANSITerminal.print_string [] "               ";
  draw_edge 47 "47" edges;
  ANSITerminal.print_string [] "             ";
  draw_edge 31 "31" edges;
  ANSITerminal.print_string [] "               ";
  draw_edge 17 "17" edges;
  ANSITerminal.print_string [] "\n";
  ANSITerminal.print_string [] "                        ";
  draw_edge 61 "\\" edges;
  ANSITerminal.print_string [] "             ";
  draw_edge 47 "/" edges;
  ANSITerminal.print_string [] "                 ";
  draw_edge 31 "\\" edges;
  ANSITerminal.print_string [] "             ";
  draw_edge 17 "/" edges;
  ANSITerminal.print_string [] "\n";
  ANSITerminal.print_string [] "                        ";
  draw_node 46 "46" nodes;
  draw_edge 53 "---53--" edges;
  draw_node 36 "36" nodes;
  draw_tile 11 "  D12 Sheep 11   " tiles;
  draw_node 25 "25" nodes;
  draw_edge 22 "---22--" edges;
  draw_node 15 "15" nodes;
  ANSITerminal.print_string [] "\n";
  ANSITerminal.print_string [] "                                      ";
  draw_edge 48 "\\" edges;
  ANSITerminal.print_string [] "                 ";
  draw_edge 32 "/" edges;
  ANSITerminal.print_string [] "\n";
  ANSITerminal.print_string [] "                                      ";
  draw_edge 48 "48" edges;
  ANSITerminal.print_string [] "               ";
  draw_edge 32 "32" edges;
  ANSITerminal.print_string [] "\n";
  ANSITerminal.print_string []
    "                                        ";
  draw_edge 48 "\\" edges;
  ANSITerminal.print_string [] "             ";
  draw_edge 32 "/" edges;
  ANSITerminal.print_string [] "\n";
  ANSITerminal.print_string []
    "                                        ";
  draw_node 37 "37" nodes;
  draw_edge 38 "---38--" edges;
  draw_node 26 "26" nodes;
  ANSITerminal.print_string [] "\n"
