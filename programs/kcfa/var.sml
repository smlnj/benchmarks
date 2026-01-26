(* var.sml
 *
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure Var :> sig

    type t

    val new : string -> t

    val toString : t -> string

    val compare : t * t -> order

    structure Map : ORD_MAP where type Key.ord_key = t

  end = struct

    type t = string

    fun new (x : string) = x

    fun toString (x : t) = x

    val compare = String.compare

    structure Map = RedBlackMapFn (
      struct
	type ord_key = t
	val compare = compare
      end)

  end
