(* main.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure Main : BMARK =
  struct

    val name = "pingpong"

    fun testit outS = (
          CML.run (fn () => PingPong.run 10);
          print "ok\n")

    fun doit () = CML.run (fn () => PingPong.run 100000000)

  end
