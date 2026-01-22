(* main.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure Main : BMARK =
  struct

    val name = "nucleic"

    val results : string list = []

    fun loop n = if n <= 0 then () else (Nucleic.anticodon_length (); loop (n - 1))

    fun doit () = loop 5000

    fun testit () = Log.say [Int.toString (Nucleic.anticodon_length ()), "\n"]

  end
