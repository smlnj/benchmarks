(* main.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure Main : BMARK =
  struct

    val name = "aobench"

    fun savePPM (fname, wid, ht, img) = let
          val outS = BinIO.openOut fname
          in
            BinIO.output (outS, Byte.stringToBytes (concat [
                "P6\n", Int.toString wid, " ", Int.toString ht, "\n255\n"
              ]));
            Word8Array.app (fn b => BinIO.output1 (outS, b)) img;
            BinIO.closeOut outS
          end

    fun doit () = ignore (AOBench.render{wid = 512, ht = 512, nSubsamples = 3})

    fun testit () = (
          savePPM (
            "out.ppm", 256, 256,
            AOBench.render{wid = 256, ht = 256, nSubsamples = 2});
          Log.print "OK\n")

    val results = ["out.ppm"]

  end
