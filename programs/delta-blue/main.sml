(* main.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure Main : BMARK =
  struct

    val name = "delta-blue"

    fun repeat n f = let
          fun lp 0 = ()
            | lp i = (f(); lp (i-1))
          in
            lp n
          end

    fun runOnce () = (DeltaBlue.chainTest 100; DeltaBlue.projectionTest 100)

    fun doit () = repeat 100 runOnce

    fun testit outS = ()

  end
