(* main.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure Main : BMARK =
  struct

    val name = "black-scholes"

    val results : string list = []

    (* preload the data for the timing tests; the "large" data set has 64K entries;
     * we scale that up to 2M entries by concatenating the list.
     *)
    val data = let
          val data = BlackScholes.readData "DATA/simlarge.txt"
          in
            List.concat(List.tabulate(32, fn _ => data))
          end

    fun testit () = let
          fun f opt = Log.say [Real.toString(BlackScholes.price opt), "\n"]
          in
            List.app f (BlackScholes.readData "DATA/simsmall.txt")
          end

    fun doOne () = let
          val prices = List.map BlackScholes.price data
	  in
	    Log.say [Real.toString(hd prices), "\n"]
	  end

    fun loop n = if n <= 0 then () else (doOne (); loop (n - 1))

    fun doit () = loop 10

  end
