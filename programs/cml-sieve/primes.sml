(* primes.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *
 * Compute primes using the sieve of Eratosthenes.
 *)

structure Primes : sig

    val nthPrime : int -> int

  end = struct

    fun sieve () = let
          (* the end-point of the pipeline *)
	  val primes = CML.channel ()
          (* create the source of the pipeline  *)
	  fun counter () = let
		val ch = CML.channel()
		fun count i = (CML.send(ch, i); count(i+1))
		in
		  CML.spawn (fn () => count 2);
		  ch
		end
          (* create a filter thread in the pipeline *)
	  fun filter (p, inCh) = let
                val outCh = CML.channel()
                fun loop () = let val i = CML.recv inCh
                      in
                        if ((i mod p) <> 0)
                          then CML.send (outCh, i)
                          else ();
                        loop ()
                      end
                in
                  CML.spawn loop;
                  outCh
                end
	  fun head ch = let
                val p = CML.recv ch
		in
		  CML.send (primes, p);
		  head (filter (p, ch))
		end
	  in
	    CML.spawn (fn () => head (counter ()));
	    primes
	  end

    fun nthPrime' n = let
	  val ch = sieve ()
	  fun loop 0 = CML.recv ch
	    | loop i = (CML.recv ch; loop(i-1))
	  in
	    loop (n-1)
	  end

    fun nthPrime n = if (n <= 0)
          then raise Fail "invalid argument"
          else CML.run(nthPrime', n)

  end
