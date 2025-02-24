(* rand.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This is the random number generator from the original Barnes-Hut C
 * code translated to SML.
 *)

structure Rand : RAND =
  struct

    val kA : Real64.real = 16807.0
    val kM : Real64.real = 2147483647.0

    val seed = ref 123.0

    fun srand s = (seed := Real.max(1.0, Real.min(s, 2147483647.0)))

    fun rand () = let
          val t = kA * !seed + 1.0
          val r = t - kM * Real.realFloor(t / kM)
          in
            seed := r;
            r
          end

    fun xrand (xl, xh) = let
          val r = rand ()
          in
            xl + (xh - xl) * r / 2147483647.0
          end

  end
