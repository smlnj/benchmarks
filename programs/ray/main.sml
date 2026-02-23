(* main.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *
 * Main structure for running raytracer as benchmark.
 *)

structure Main : BMARK =
  struct

    val name = "ray"

    val results = ["out.ppm"]

    fun trace () = let
	  val strm = TextIO.openIn "DATA/sphere"
	  in
	    Interface.rtInit ();
	    Interp.parse strm;
	    TextIO.closeIn strm
	  end

    fun doit () = let
          fun loop n = if n = 0
                then ()
                else (trace (); loop (n - 1))
          in
            loop 100
          end

    fun testit _ = trace ()

  end
