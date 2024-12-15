(* rand-sig.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Signature for a simple random number generator.
 *
 *)

signature RAND =
  sig

    type rand = Word.word

    val mkRandom : rand -> unit -> rand

    val norm : rand -> real

  end (* RAND *)
