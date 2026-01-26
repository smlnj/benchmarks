(* label.sml
 *
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure Label : sig

    eqtype t

    val new : unit -> t

    val toString : t -> string

    val compare : t * t -> order
    val same : t * t -> bool

  end = struct

    type t = int

    val count = ref 1

    fun new () = let val n = !count + 1 in count := n; n end

    fun toString l = "L" ^ Int.toString l

    val compare = Int.compare
    fun same (l1 : t, l2 : t) = (l1 = l2)

  end
