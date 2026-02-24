(* main.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *
 * Main structure for running the benchmark.
 *)

structure Main : BMARK =
  struct

    val name = "ray"

    val results = ["out.ppm"]

    fun trace file = let
	  val strm = TextIO.openIn file
	  in
	    Interface.rtInit ();
	    Interp.parse strm;
	    TextIO.closeIn strm
	  end

    fun doit () = let
          fun loop n = if n = 0
                then ()
                else (trace "DATA/bmark.txt"; loop (n - 1))
          in
            loop 500
          end

    fun testit _ = trace "DATA/test.txt"

  end
