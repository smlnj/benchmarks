(* main.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure Main : BMARK =
  struct

    val name = "cml-sieve"

    fun testit () = let
          val res = List.tabulate(10, fn i => Primes.nthPrime(i+1))
          in
            if ListPair.allEq (op =) (res, [2,3,5,7,11,13,17,19,23,29])
              then Log.print "OK\n"
              else Log.print "FAIL\n"
          end

    fun doit () = ignore (Primes.nthPrime 12000)

  end
