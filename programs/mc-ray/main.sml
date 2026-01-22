(* main.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure Main : BMARK =
  struct

    val name = "mc-ray"

    val results = ["out.ppm"]

    fun render () = Trace.rayTracer (RandomScene.build (150, 100, 50))

    fun doit () = ignore (render ())

    fun testit outS = let
          val img = render ()
          in
            Image.writePPM ("out.ppm", img);
            Log.print "OK\n"
          end

  end
