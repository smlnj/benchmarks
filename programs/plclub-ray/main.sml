(* main.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure Main : BMARK =
   struct

      val name = "plclub-ray"

      fun runOnce () =
         Eval.f (Program.read (TextIO.openIn "chess.gml"))
         handle _ => ()

      fun run n = let
            fun loop n = if n = 0 then () else (runOnce(); loop(n-1))
            in loop n
            end

      fun testit outS =
            TextIO.output(
              outS,
              (runOnce (); "okay\n")
                handle ex => exnMessage ex ^ "\n")

      fun doit () = run 10

   end
