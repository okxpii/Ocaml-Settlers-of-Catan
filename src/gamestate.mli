(** Where the board is created and roll dice is called  *)

type game_state
(** The game state, including players, nodes, edges, resources, and cards *)

val roll_dice : unit -> int
(** [roll_dice] returns the sum of two random dice rolls (each in the
    range 0-6) *)

val init_state : game_state
(** The board at the start of the game  *)
