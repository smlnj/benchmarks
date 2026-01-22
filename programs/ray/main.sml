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

    fun doit () = let
          fun loop n = if n = 0
                then ()
                else let
                  val strm = TextIO.openIn "DATA/sphere"
                  val _ = Interface.rtInit()
                  val _ = Interp.parse strm
                  val _ = TextIO.closeIn strm
                  in
                     loop (n - 1)
                  end
          in
            loop 100
          end

(* FIXME *)
    fun testit _ = ()

  end
