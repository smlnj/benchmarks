(* sieve.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure Sieve : sig

    val primes : int Streams.t

  end = struct

    fun countFromN n = Streams.make (n, fn () => countFromN (n+1))

    fun sift (n, strm) = let
          val (k, strm') = Streams.unfold strm
          in
            if Int.rem(k, n) = 0
              then sift (n, strm')
              else Streams.make(k, fn () => sift (n, strm'))
          end

    (* Sieve of Eratosthenes *)
    fun sieve strm = let
          val (k, strm') = Streams.unfold strm
          in
            Streams.make (k, fn () => sieve (sift (k, strm')))
          end

    val primes = sieve (countFromN 2)

  end
