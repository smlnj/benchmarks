(* main.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure Main : BMARK =
  struct

    val name = "pidigits"

    val results = []

    fun display n = PiDigits.generate n

    fun testit () = display 30

    fun doit () = display 2000

  end
