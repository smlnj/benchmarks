(* rand-sig.sml
 *
 * COPYRIGHT (c) 2019 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

signature RAND =
  sig

    val init : word -> unit

    val rand : unit -> Real64.real

    val randInt : int -> int

  end
