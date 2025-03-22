(* main.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure Main : BMARK =
  struct

    val name = "mlyacc"

    fun loop n =
      if n <= 0 then () else (ParseGen.parseGen "DATA/ml.grm"; loop (n - 1))

    fun doit() = loop 200

    fun testit _ = ParseGen.parseGen "DATA/ml.grm"

  end
