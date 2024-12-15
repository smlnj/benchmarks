(* main.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure Main : BMARK =
  struct

    val name = "lexgen"

    fun doit () = LexGen.lexGen "DATA/ml.lex"

(* FIXME: need to generate output *)
    fun testit outS = LexGen.lexGen "DATA/ml.lex"

  end (* Main *)
