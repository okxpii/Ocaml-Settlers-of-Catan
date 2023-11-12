(* Test Plan: The Board and Command modules are manually tested here.
   The other modules-- Gamestate, and Player-- are not tested
   automatically since they are mostly used in the terminal, or are
   helper type_to_string functions for other functions which we do test.
   Instead, we throughly test the Commamand and Board modules. First, we
   used black box testing and test-driven development while implementing
   the Board module. This meant testing all the getters and setters for
   the nodes, edges, and tiles, as well as getting resources. For the
   command file, we also tested the parse_string file, to make sure
   commands correctly route to right commands for the game loop. After
   that, we used Bisect to make sure we were'nt undertesting Board and
   Command. Command has 100% coverage and Board is at 97%, with the only
   functions not being covered being string helper functions. *)

open OUnit2
open Catan
open Board
open Gamestate
open Player
open Command

let get_tile_test
    (name : string)
    (ind : int)
    (tiles : Board.tile list)
    (expected_output : Board.tile) : test =
  name >:: fun _ -> assert_equal expected_output (get_tile ind tiles)

let print_node n = string_of_node n

let get_node_test
    (name : string)
    (ind : int)
    (nodes : Board.node list)
    (expected_output : Board.node) : test =
  name >:: fun _ ->
  assert_equal expected_output (get_node ind nodes) ~printer:print_node

let get_edge_test
    (name : string)
    (ind : int)
    (edges : Board.edge list)
    (expected_output : Board.edge) : test =
  name >:: fun _ -> assert_equal expected_output (get_edge ind edges)

let printer_edges l = String.concat "; " (List.map string_of_edge l)

let build_road_test
    (name : string)
    (ind : int)
    (player : Player.player)
    (edges : Board.edge list)
    (expected_output : Board.edge list) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (build_road ind player edges)
    ~printer:printer_edges

let printer_nodes l = String.concat "; " (List.map string_of_node l)

let build_settlement_test
    (name : string)
    (ind : int)
    (player : Player.player)
    (nodes : Board.node list)
    (expected_output : Board.node list) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (build_settlement ind player nodes)
    ~printer:printer_nodes

let string_of_tuple (i, r) =
  Printf.sprintf "(%d, %s)" i (string_resource r)

let printer_resource l = String.concat "; " (List.map string_of_tuple l)

let get_resource_test
    (name : string)
    (input : int)
    (expected_output : (int * resource) list) : test =
  name >:: fun _ ->
  assert_equal expected_output (get_resource input)
    ~printer:printer_resource

(* let display_even_test (name : string) (n1 : string) (n2 : string) (n3
   : string) (n4 : string) (n5 : string) (n6 : string) (expected_output
   : string) : test = name >:: fun _ -> assert_equal expected_output
   (display_even n1 n2 n3 n4 n5 n6) ~printer:(fun x -> x)

   let display_odd_test (name : string) (n1 : string) (n2 : string) (n3
   : string) (n4 : string) (n5 : string) (n6 : string) (n7 : string)
   (expected_output : string) : test = name >:: fun _ -> assert_equal
   expected_output (display_odd n1 n2 n3 n4 n5 n6 n7) ~printer:(fun x ->
   x) *)

let draw_test
    (name : string)
    (tiles : Board.tile list)
    (nodes : Board.node list)
    (edges : Board.edge list)
    (expected_output : unit) : test =
  name >:: fun _ ->
  assert_equal expected_output (draw_board tiles nodes edges)

let player_red =
  {
    player_color = Red;
    resources = [];
    development_cards = [];
    score = 0;
    num_settlements = 0;
    num_cities = 0;
    num_roads = 0;
    has_rolled = false;
    played_card = false;
  }

let player_blue =
  {
    player_color = Blue;
    resources = [];
    development_cards = [];
    score = 0;
    num_settlements = 0;
    num_cities = 0;
    num_roads = 0;
    has_rolled = false;
    played_card = false;
  }

let tile_test = tile_list
let node_test = node_list
let edge_test = edge_list
let wheat_test = test_resource "Wheat"
let desert_test = test_resource "Desert"
let brick_test = test_resource "Brick"
let sheep_test = test_resource "Sheep"
let wood_test = test_resource "Wood"
let ore_test = test_resource "Ore"
let test_edges_basic = build_road 4 player_red Board.edge_list

let test_edges_sequence =
  let settle_4 = build_road 4 player_red Board.edge_list in
  let settle_35 = build_road 35 player_red settle_4 in
  let settle_17 = build_road 17 player_red settle_35 in
  build_road 53 player_red settle_17

let test_nodes_basic = build_settlement 4 player_red Board.node_list

let test_nodes_sequence =
  let settle_4 = build_settlement 4 player_red Board.node_list in
  let settle_35 = build_settlement 35 player_red settle_4 in
  let settle_17 = build_settlement 17 player_red settle_35 in
  build_settlement 53 player_red settle_17

let board_tests =
  [
    get_tile_test "Check tile of index 0" 0 tile_test
      (List.nth tile_test 0);
    get_tile_test "Check tile of index 1" 1 tile_test
      (List.nth tile_test 1);
    get_tile_test "Check tile of index 2" 2 tile_test
      (List.nth tile_test 2);
    get_tile_test "Check tile of index 3" 3 tile_test
      (List.nth tile_test 3);
    get_tile_test "Check tile of index 4" 4 tile_test
      (List.nth tile_test 4);
    get_tile_test "Check tile of index 5" 5 tile_test
      (List.nth tile_test 5);
    get_tile_test "Check tile of index 6" 6 tile_test
      (List.nth tile_test 6);
    get_tile_test "Check tile of index 7" 7 tile_test
      (List.nth tile_test 7);
    get_tile_test "Check tile of index 8" 8 tile_test
      (List.nth tile_test 8);
    get_tile_test "Check tile of index 9" 9 tile_test
      (List.nth tile_test 9);
    get_tile_test "Check tile of index 10" 10 tile_test
      (List.nth tile_test 10);
    get_tile_test "Check tile of index 11" 11 tile_test
      (List.nth tile_test 11);
    get_tile_test "Check tile of index 12" 12 tile_test
      (List.nth tile_test 12);
    get_tile_test "Check tile of index 13" 13 tile_test
      (List.nth tile_test 13);
    get_tile_test "Check tile of index 14" 14 tile_test
      (List.nth tile_test 14);
    get_tile_test "Check tile of index 15" 15 tile_test
      (List.nth tile_test 15);
    get_tile_test "Check tile of index 16" 16 tile_test
      (List.nth tile_test 16);
    get_tile_test "Check tile of index 17" 17 tile_test
      (List.nth tile_test 17);
    get_tile_test "Check tile of index 18" 18 tile_test
      (List.nth tile_test 18);
    get_node_test "Check node of index 0" 0 node_test
      (List.nth node_test 0);
    get_node_test "Check node of index 1" 1 node_test
      (List.nth node_test 1);
    get_node_test "Check node of index 2" 2 node_test
      (List.nth node_test 2);
    get_node_test "Check node of index 3" 3 node_test
      (List.nth node_test 3);
    get_node_test "Check node of index 4" 4 node_test
      (List.nth node_test 4);
    get_node_test "Check node of index 5" 5 node_test
      (List.nth node_test 5);
    get_node_test "Check node of index 6" 6 node_test
      (List.nth node_test 6);
    get_node_test "Check node of index 7" 7 node_test
      (List.nth node_test 7);
    get_node_test "Check node of index 8" 8 node_test
      (List.nth node_test 8);
    get_node_test "Check node of index 9" 9 node_test
      (List.nth node_test 9);
    get_node_test "Check node of index 10" 10 node_test
      (List.nth node_test 10);
    get_node_test "Check node of index 11" 11 node_test
      (List.nth node_test 11);
    get_node_test "Check node of index 12" 12 node_test
      (List.nth node_test 12);
    get_node_test "Check node of index 13" 13 node_test
      (List.nth node_test 13);
    get_node_test "Check node of index 14" 14 node_test
      (List.nth node_test 14);
    get_node_test "Check node of index 15" 15 node_test
      (List.nth node_test 15);
    get_node_test "Check node of index 16" 16 node_test
      (List.nth node_test 16);
    get_node_test "Check node of index 17" 17 node_test
      (List.nth node_test 17);
    get_node_test "Check node of index 18" 18 node_test
      (List.nth node_test 18);
    get_node_test "Check node of index 19" 19 node_test
      (List.nth node_test 19);
    get_node_test "Check node of index 20" 20 node_test
      (List.nth node_test 20);
    get_node_test "Check node of index 21" 21 node_test
      (List.nth node_test 21);
    get_node_test "Check node of index 22" 22 node_test
      (List.nth node_test 22);
    get_node_test "Check node of index 23" 23 node_test
      (List.nth node_test 23);
    get_node_test "Check node of index 24" 24 node_test
      (List.nth node_test 24);
    get_node_test "Check node of index 25" 25 node_test
      (List.nth node_test 25);
    get_node_test "Check node of index 26" 26 node_test
      (List.nth node_test 26);
    get_node_test "Check node of index 27" 27 node_test
      (List.nth node_test 27);
    get_node_test "Check node of index 28" 28 node_test
      (List.nth node_test 28);
    get_node_test "Check node of index 29" 29 node_test
      (List.nth node_test 29);
    get_node_test "Check node of index 30" 30 node_test
      (List.nth node_test 30);
    get_node_test "Check node of index 31" 31 node_test
      (List.nth node_test 31);
    get_node_test "Check node of index 32" 32 node_test
      (List.nth node_test 32);
    get_node_test "Check node of index 33" 33 node_test
      (List.nth node_test 33);
    get_node_test "Check node of index 34" 34 node_test
      (List.nth node_test 34);
    get_node_test "Check node of index 35" 35 node_test
      (List.nth node_test 35);
    get_node_test "Check node of index 36" 36 node_test
      (List.nth node_test 36);
    get_node_test "Check node of index 37" 37 node_test
      (List.nth node_test 37);
    get_node_test "Check node of index 38" 38 node_test
      (List.nth node_test 38);
    get_node_test "Check node of index 39" 39 node_test
      (List.nth node_test 39);
    get_node_test "Check node of index 40" 40 node_test
      (List.nth node_test 40);
    get_node_test "Check node of index 41" 41 node_test
      (List.nth node_test 41);
    get_node_test "Check node of index 42" 42 node_test
      (List.nth node_test 42);
    get_node_test "Check node of index 43" 43 node_test
      (List.nth node_test 43);
    get_node_test "Check node of index 44" 44 node_test
      (List.nth node_test 44);
    get_node_test "Check node of index 45" 45 node_test
      (List.nth node_test 45);
    get_node_test "Check node of index 46" 46 node_test
      (List.nth node_test 46);
    get_node_test "Check node of index 47" 47 node_test
      (List.nth node_test 47);
    get_node_test "Check node of index 48" 48 node_test
      (List.nth node_test 48);
    get_node_test "Check node of index 49" 49 node_test
      (List.nth node_test 49);
    get_node_test "Check node of index 50" 50 node_test
      (List.nth node_test 50);
    get_node_test "Check node of index 51" 51 node_test
      (List.nth node_test 51);
    get_node_test "Check node of index 52" 52 node_test
      (List.nth node_test 52);
    get_node_test "Check node of index 53" 53 node_test
      (List.nth node_test 53);
    get_edge_test "Check edge of index 0" 0 edge_test
      (List.nth edge_test 0);
    get_edge_test "Check edge of index 1" 1 edge_test
      (List.nth edge_test 1);
    get_edge_test "Check edge of index 2" 2 edge_test
      (List.nth edge_test 2);
    get_edge_test "Check edge of index 3" 3 edge_test
      (List.nth edge_test 3);
    get_edge_test "Check edge of index 4" 4 edge_test
      (List.nth edge_test 4);
    get_edge_test "Check edge of index 5" 5 edge_test
      (List.nth edge_test 5);
    get_edge_test "Check edge of index 6" 6 edge_test
      (List.nth edge_test 6);
    get_edge_test "Check edge of index 7" 7 edge_test
      (List.nth edge_test 7);
    get_edge_test "Check edge of index 8" 8 edge_test
      (List.nth edge_test 8);
    get_edge_test "Check edge of index 9" 9 edge_test
      (List.nth edge_test 9);
    get_edge_test "Check edge of index 10" 10 edge_test
      (List.nth edge_test 10);
    get_edge_test "Check edge of index 11" 11 edge_test
      (List.nth edge_test 11);
    get_edge_test "Check edge of index 12" 12 edge_test
      (List.nth edge_test 12);
    get_edge_test "Check edge of index 13" 13 edge_test
      (List.nth edge_test 13);
    get_edge_test "Check edge of index 14" 14 edge_test
      (List.nth edge_test 14);
    get_edge_test "Check edge of index 15" 15 edge_test
      (List.nth edge_test 15);
    get_edge_test "Check edge of index 16" 16 edge_test
      (List.nth edge_test 16);
    get_edge_test "Check edge of index 17" 17 edge_test
      (List.nth edge_test 17);
    get_edge_test "Check edge of index 18" 18 edge_test
      (List.nth edge_test 18);
    get_edge_test "Check edge of index 19" 19 edge_test
      (List.nth edge_test 19);
    get_edge_test "Check edge of index 20" 20 edge_test
      (List.nth edge_test 20);
    get_edge_test "Check edge of index 21" 21 edge_test
      (List.nth edge_test 21);
    get_edge_test "Check edge of index 22" 22 edge_test
      (List.nth edge_test 22);
    get_edge_test "Check edge of index 23" 23 edge_test
      (List.nth edge_test 23);
    get_edge_test "Check edge of index 24" 24 edge_test
      (List.nth edge_test 24);
    get_edge_test "Check edge of index 25" 25 edge_test
      (List.nth edge_test 25);
    get_edge_test "Check edge of index 26" 26 edge_test
      (List.nth edge_test 26);
    get_edge_test "Check edge of index 27" 27 edge_test
      (List.nth edge_test 27);
    get_edge_test "Check edge of index 28" 28 edge_test
      (List.nth edge_test 28);
    get_edge_test "Check edge of index 29" 29 edge_test
      (List.nth edge_test 29);
    get_edge_test "Check edge of index 30" 30 edge_test
      (List.nth edge_test 30);
    get_edge_test "Check edge of index 31" 31 edge_test
      (List.nth edge_test 31);
    get_edge_test "Check edge of index 32" 32 edge_test
      (List.nth edge_test 32);
    get_edge_test "Check edge of index 33" 33 edge_test
      (List.nth edge_test 33);
    get_edge_test "Check edge of index 34" 34 edge_test
      (List.nth edge_test 34);
    get_edge_test "Check edge of index 35" 35 edge_test
      (List.nth edge_test 35);
    get_edge_test "Check edge of index 36" 36 edge_test
      (List.nth edge_test 36);
    get_edge_test "Check edge of index 37" 37 edge_test
      (List.nth edge_test 37);
    get_edge_test "Check edge of index 38" 38 edge_test
      (List.nth edge_test 38);
    get_edge_test "Check edge of index 39" 39 edge_test
      (List.nth edge_test 39);
    get_edge_test "Check edge of index 40" 40 edge_test
      (List.nth edge_test 40);
    get_edge_test "Check edge of index 41" 41 edge_test
      (List.nth edge_test 41);
    get_edge_test "Check edge of index 42" 42 edge_test
      (List.nth edge_test 42);
    get_edge_test "Check edge of index 43" 43 edge_test
      (List.nth edge_test 43);
    get_edge_test "Check edge of index 44" 44 edge_test
      (List.nth edge_test 44);
    get_edge_test "Check edge of index 45" 45 edge_test
      (List.nth edge_test 45);
    get_edge_test "Check edge of index 46" 46 edge_test
      (List.nth edge_test 46);
    get_edge_test "Check edge of index 47" 47 edge_test
      (List.nth edge_test 47);
    get_edge_test "Check edge of index 48" 48 edge_test
      (List.nth edge_test 48);
    get_edge_test "Check edge of index 49" 49 edge_test
      (List.nth edge_test 49);
    get_edge_test "Check edge of index 50" 50 edge_test
      (List.nth edge_test 50);
    get_edge_test "Check edge of index 51" 51 edge_test
      (List.nth edge_test 51);
    get_edge_test "Check edge of index 52" 52 edge_test
      (List.nth edge_test 52);
    get_edge_test "Check edge of index 53" 53 edge_test
      (List.nth edge_test 53);
    get_edge_test "Check edge of index 54" 54 edge_test
      (List.nth edge_test 54);
    get_edge_test "Check edge of index 55" 55 edge_test
      (List.nth edge_test 55);
    get_edge_test "Check edge of index 56" 56 edge_test
      (List.nth edge_test 56);
    get_edge_test "Check edge of index 57" 57 edge_test
      (List.nth edge_test 57);
    get_edge_test "Check edge of index 58" 58 edge_test
      (List.nth edge_test 58);
    get_edge_test "Check edge of index 59" 59 edge_test
      (List.nth edge_test 59);
    get_edge_test "Check edge of index 60" 60 edge_test
      (List.nth edge_test 60);
    get_edge_test "Check edge of index 61" 61 edge_test
      (List.nth edge_test 61);
    get_edge_test "Check edge of index 62" 62 edge_test
      (List.nth edge_test 62);
    get_edge_test "Check edge of index 63" 63 edge_test
      (List.nth edge_test 63);
    get_edge_test "Check edge of index 64" 64 edge_test
      (List.nth edge_test 64);
    get_edge_test "Check edge of index 65" 65 edge_test
      (List.nth edge_test 65);
    get_edge_test "Check edge of index 66" 66 edge_test
      (List.nth edge_test 66);
    get_edge_test "Check edge of index 67" 67 edge_test
      (List.nth edge_test 67);
    get_edge_test "Check edge of index 68" 68 edge_test
      (List.nth edge_test 68);
    get_edge_test "Check edge of index 69" 69 edge_test
      (List.nth edge_test 69);
    get_edge_test "Check edge of index 70" 70 edge_test
      (List.nth edge_test 70);
    get_edge_test "Check edge of index 71" 71 edge_test
      (List.nth edge_test 71);
    build_road_test "Basic test for build_road" 4 player_red edge_test
      test_edges_basic;
    build_road_test "Complex test for build_road" 43 player_red
      test_edges_sequence
      (build_road 43 player_red test_edges_sequence);
    build_road_test "Multiplayer test for build_road" 53 player_blue
      test_edges_sequence test_edges_sequence;
    build_road_test "Out of index test for build_road" 72 player_red
      test_edges_sequence test_edges_sequence;
    build_settlement_test "Basic test for build_settlement" 4 player_red
      node_test test_nodes_basic;
    build_settlement_test "Complex test for build_settlement" 43
      player_red test_nodes_sequence
      (build_settlement 43 player_red test_nodes_sequence);
    build_settlement_test "Multiplayer test for build_settlement" 53
      player_blue test_nodes_sequence test_nodes_sequence;
    build_settlement_test "Out of index test for build_settlement" 54
      player_red test_nodes_sequence test_nodes_sequence;
    get_resource_test "Check when dice roll has one resource" 11
      [ (15, wood_test); (16, wheat_test) ];
    get_resource_test "Check when return has length one" 2
      [ (13, wood_test) ];
    get_resource_test "Check when dice roll has two different resources"
      3
      [ (4, sheep_test); (12, brick_test) ];
    draw_test "Check" Board.tile_list Board.node_list Board.edge_list ();
  ]

let string_of_command cmd =
  match cmd with
  | Empty -> "empty"
  | BuyCard -> "buy card"
  | CheckCards -> "check cards"
  | CheckScore -> "check score"
  | CheckRoads -> "check roads"
  | CheckSettlements -> "check settlements"
  | CheckResources -> "check resources"
  | EndTurn -> "end turn"
  | PlayCard -> "play card"
  | BuildRoad -> "build road"
  | Settle -> "settle"
  | Roll -> "roll"
  | Quit -> "quit"
  | Start -> "start"
  | Invalid -> "invalid"

let parse_string_test
    (name : string)
    (input : string)
    (expected_output : Command.command) : test =
  name >:: fun _ ->
  assert_equal expected_output (parse_string input)
    ~printer:string_of_command

let command_tests =
  [
    parse_string_test "Check Start command" "start" Start;
    parse_string_test "Check Empty command" "" Empty;
    parse_string_test "Check BuyCard command" "buy card" BuyCard;
    parse_string_test "Check CheckCards command" "check cards"
      CheckCards;
    parse_string_test "Check CheckScore command" "check score"
      CheckScore;
    parse_string_test "Check CheckRoads command" "check roads"
      CheckRoads;
    parse_string_test "Check CheckSettlements command"
      "check settlements" CheckSettlements;
    parse_string_test "Check CheckResources command" "check resources"
      CheckResources;
    parse_string_test "Check EndTurn command" "end turn" EndTurn;
    parse_string_test "Check PlayCard command" "play card" PlayCard;
    parse_string_test "Check BuildRoad command" "build road" BuildRoad;
    parse_string_test "Check Settle command" "settle" Settle;
    parse_string_test "Check Roll command" "roll" Roll;
    parse_string_test "Check Quit command" "quit" Quit;
    parse_string_test "Check Invalid command" "sdfjos" Invalid;
  ]

let suite =
  "test suite for game" >::: List.flatten [ board_tests; command_tests ]

let _ = run_test_tt_main suite
