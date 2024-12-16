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

    val srand : real -> unit

    val xrand : real * real -> real

  end (* RAND *)
