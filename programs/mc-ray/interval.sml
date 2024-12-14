(* interval.sml
 *
 * COPYRIGHT (c) 2019 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure Interval : sig

    type t = (Real64.real * Real64.real)

    val within : Real64.real * t -> bool

    val toString : t -> string

  end = struct

    type t = (Real64.real * Real64.real)

    fun within (t, (min, max) : t) = (min <= t) andalso (t <= max)

    fun toString ((min, max) : t) = String.concat[
	    "[", Real64.toString min, " .. ", Real64.toString max, "]"
	  ]

  end

