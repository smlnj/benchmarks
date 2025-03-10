(* main.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure Main : BMARK =
  struct

    val name = "black-scholes"

    (* preload the data for the timing tests; the "large" data set has 64K entries;
     * we scale that up to 2M entries by concatenating the list.
     *)
    val data = let
          val data = BlackScholes.readData "DATA/simlarge.txt"
          in
            List.concat(List.tabulate(32, fn _ => data))
          end

    fun testit outS = let
          val options = BlackScholes.readData "DATA/simsmall.txt"
          fun f opt = TextIO.output(outS, Real.toString(BlackScholes.price opt))
          in
            List.app f (BlackScholes.readData "DATA/simsmall.txt")
          end

    fun doit () = let
          val prices = List.map BlackScholes.price data
	  in
	    TextIO.print (Real.toString(hd prices) ^ "\n")
	  end

  end
