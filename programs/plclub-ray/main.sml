(* main.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure Main : BMARK =
   struct

    val name = "plclub-ray"

    val results : string list = ["chess.ppm"]

    fun runOnce gmlFile =
       Eval.f (Program.read (TextIO.openIn("DATA" ^ gmlFile)))
       handle _ => ()

    fun run n = let
          fun loop n = if n = 0 then () else (runOnce "bmark.gml"; loop(n-1))
          in loop n
          end

    fun testit () =
          Log.print (
            (runOnce "test.gml"; "OK\n")
              handle ex => exnMessage ex ^ "\n")

    fun doit () = run 10

   end
