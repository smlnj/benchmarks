(* main.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure Main : BMARK =
  struct

    val name = "sieve"

    fun testit outS = let
	  fun thd () = let
                val n = 100
		val p = Sieve.nthPrime n (* should be 541 *)
		in
		  TextIO.output(outS, concat[
		      "Prime(", Int.toString n, ") = ",
		      Int.toString p, "\n"
		    ])
		end
	  in
	    CML.run thd
	  end

    fun doit () = let
          fun thd () = ignore (Sieve.nthPrime 12000)
          in
            CML.run thd
          end

  end
