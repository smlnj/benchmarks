(* main.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure Main : BMARK =
  struct
    val name = "nucleic"

    fun loop n =
      if n <= 0 then () else (Nucleic.anticodon_length (); loop (n - 1))

    fun doit () = loop 1000

    fun testit strm = TextIO.output(strm, concat[
	    Int.toString (Nucleic.anticodon_length ()), "\n"
	  ])

  end
