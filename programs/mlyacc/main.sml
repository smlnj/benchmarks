(* main.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure Main : BMARK =
  struct

    val name = "mlyacc"

    fun doit() = ParseGen.parseGen "DATA/ml.grm"

    fun testit _ = ParseGen.parseGen "DATA/ml.grm"

  end
