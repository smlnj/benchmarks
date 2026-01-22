(* main.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure Main : BMARK =
  struct

    val name = "logic"

    val results : string list = []

    exception Done

    fun testit () = Data.exists(fn Z => Data.solution2 Z (fn () => raise Done))
	  handle Done => Log.print "OK\n"

    fun loop n = if n <= 0
	  then ()
          else (
	    Data.exists(fn Z => Data.solution2 Z (fn () => raise Done))
              handle Done => ();
            loop (n - 1))

    fun doit () = loop 60

  end; (* Main *)
