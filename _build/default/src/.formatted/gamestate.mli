type game_state
(*May need to copy entire record if used in a different file*)

val roll_dice : unit -> int
(** [roll_dice] returns the sum of two random dice rolls (each in the
    range 0-6) *)

val init_state : game_state
