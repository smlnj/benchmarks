(* main.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure Main : BMARK =
  struct

    val name = "lexgen"

    val results = ["ml.lex.sml"]

    fun loop n =
      if n <= 0 then () else (LexGen.lexGen "DATA/ml.lex"; loop (n - 1))

    fun doit () = loop 500

(* FIXME: need to generate output *)
    fun testit outS = LexGen.lexGen "DATA/ml.lex"

  end (* Main *)
