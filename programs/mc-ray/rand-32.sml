(* rand-32.sml
 *
 * COPYRIGHT (c) 2019 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Park-Miller RNG (MINSTD) for 32-bit architectures.  This implementation is
 * from
 *	https://en.wikipedia.org/wiki/Lehmer_random_number_generator
 *)

structure Rand : RAND =
  struct


    val state : Word32.word ref = ref 0w1234567

    fun init 0w0 = (state := 0w1234567)
      | init w = (state := w)

    val n : Word32.word = 0wx7fffffff
    val g : Word32.word = 0w48271
    val n_div_g = n div g
    val n_mod_g = n mod g

    fun randWord32 () = let
        (* Indirectly compute state*G%N.
         *
	 * Let:
	 *   div = state/(N/G)
	 *   rem = state%(N/G)
         *
	 * Then:
	 *   rem + div*(N/G) == state
	 *   rem*G + div*(N/G)*G == state*G
         *
	 * Now:
	 *   div*(N/G)*G == div*(N - N%G) === -div*(N%G)  (mod N)
         *
	 * Therefore:
	 *   rem*G - div*(N%G) === state*G  (mod N)
         *
	 * Add N if necessary so that the result is between 1 and N-1.
	 *)
          val r = !state
          val d = r div n_div_g
          val r = r mod n_div_g
          val a = r * g
          val b = d * n_mod_g
          val r' = if (a > b) then (a - b) else (a + (n - b))
          in
	    state := r';
	    r'
          end

    val scale : Real64.real = 1.0 / 2147483647.0

    fun rand () = scale * Real64.fromLargeInt (Word32.toLargeIntX (randWord32 ()))

    fun randInt n = if (n <= 1)
	  then 1
	  else Word32.toIntX(Word32.mod(randWord32(), Word32.fromInt n))

  end
