(* main.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure Main : BMARK =
  struct

    val name = "id-ray"

    val results = ["out.ppm"]

    fun testit () = (Ray.ray ("out.ppm", 256); Log.print "OK\n")
          handle _ => Log.print "FAIL\n"

    fun doit () = Ray.ray ("out.ppm", 1024)

  end
