(* main.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure Main : BMARK =
  struct

    val name = "stream-sieve"

    val results : string list = []

    fun testit () = let
          val res = Streams.take (Sieve.primes, 10)
          in
            if ListPair.allEq (op =) (res, [2,3,5,7,11,13,17,19,23,29])
              then Log.print "OK\n"
              else Log.print "FAIL\n"
          end

    fun doit () = ignore (Streams.get (Sieve.primes, 20000))

  end
