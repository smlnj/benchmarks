(* main.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure Main : BMARK =
  struct

    val name = "mlyacc"

    fun runOnce () = ParseGen.parseGen "DATA/ml.grm"

    fun doit () = let
	  fun lp 0 = ()
	    | lp i = (runOnce(); lp(i-1))
	  in
	    lp 250
	  end

    fun testit _ = ParseGen.parseGen "DATA/ml.grm"

  end
