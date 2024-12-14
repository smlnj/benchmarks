(* main.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure Main : BMARK =
  struct

    val name = "mc-ray"

    fun render () = Trace.rayTracer (RandomScene.build (150, 100, 50))

    fun doit () = ignore (render ())

    fun testit outS = let
          val img = render ()
          in
            Image.writePPM ("mc-ray-output.ppm", img);
            TextIO.output (outS, "ok\n")
          end

  end
