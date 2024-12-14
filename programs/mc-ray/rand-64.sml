(* rand-64.sml
 *
 * COPYRIGHT (c) 2019 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Park-Miller RNG (MINSTD) for 64-bit architectures.  This implementation is
 * from
 *	https://en.wikipedia.org/wiki/Lehmer_random_number_generator
 *)

structure Rand : RAND =
  struct

    (* mask to 31-bits *)
    val mask : word = 0wx7fffffff

    val state : Word.word ref = ref 0w1234567

    fun init 0w0 = (state := 0w1234567)
      | init w = (state := Word.andb(w, 0wx7fffffff))

    fun randWord () = let
          val product = !state * 0w48271
          val x = Word.andb(product, mask) + Word.>>(product, 0w31)
          val x = Word.andb(x, mask) + Word.>>(x, 0w31)
	  in
	    state := x;
	    x
	  end

    val scale : Real64.real = 1.0 / 2147483647.0 (* 2147483647 == 07fffffff *)

    fun rand () = scale * Real64.fromLargeInt (Word.toLargeIntX (randWord ()))

    fun randInt n = if (n <= 1)
	  then 1
	  else Word.toIntX(Word.mod(randWord(), Word.fromInt n))

  end
