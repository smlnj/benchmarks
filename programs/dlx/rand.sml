(* rand.sml
 *)

structure Rand : sig

    val rand : unit -> word

  end = struct

    val seed : word ref = ref 0w13

    (* From page 284 of Numerical Recipes in C. *)
    fun rand () : word = let
          val res = 0w1664525 * !seed + 0w1013904223
          val _ = seed := res
          in
            res
          end

  end
