(* rand.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Park-Miller RNG (MINSTD) for 64-bit architectures.  This implementation is
 * from
 *      https://en.wikipedia.org/wiki/Lehmer_random_number_generator
 *)

structure Rand : RAND =
  struct

    type rand = Word.word (* restricted to 31 bits *)

    (* mask to 31-bits *)
    val mask : word = 0wx7fffffff

    val seed : rand ref = ref 0w123

    fun srand s = (seed := Word.andb(s, mask))

    fun rand () = let
          val product = !seed * 0w48271
          val x = Word.andb(product, mask) + Word.>>(product, 0w31)
          val x = Word.andb(x, mask) + Word.>>(x, 0w31)
          in
            seed := x;
            x
          end

    fun xrand (xl, xh) = let
          val r = Real64.fromLargeInt (Word.toLargeIntX (rand ()))
          in
            xl + (((xh - xl) * r) / 2147483647.0)
          end

  end
