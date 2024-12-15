(* main.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure Main : BMARK =
  struct
    val name = "nucleic"

    fun doit () = (Nucleic.anticodon_length (); ())

    fun testit strm = TextIO.output(strm, concat[
	    Int.toString (Nucleic.anticodon_length ()), "\n"
	  ])

  end
