(* main.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure Main : BMARK =
  struct

    structure A = Array

    val name = "fannkuch"

    val results = []

    fun for (start, stop, f) = let
          fun lp i = if (i <= stop)
                then (f i; lp(i+1))
                else ()
          in
            lp start
          end

    fun countFlips perm = let
          (* count the number of flips before '0' is the first element *)
          fun loop c = let
                val k = A.sub (perm, 0)
                in
                  if k = 0 then c
                    else (
                      (* reverse the elements [0..k] *)
                      for (0, k div 2, fn i => let
                         val k_i = k - i
                         val perm_i = A.sub (perm, i)
                         in
                           A.update (perm, i, A.sub (perm, k_i));
                           A.update (perm, k_i, perm_i)
                         end);
                      loop (c + 1))
                end
          in
            loop 0
          end

    fun fannkuch n = let
          val perm = A.array (n, 0)
          val perm1 = A.tabulate (n, fn i => i)
          val count = A.array (n, 0)
          fun loop (0, maxFlips, checkSum, _) = (maxFlips, checkSum)
            | loop (r, maxFlips, checkSum, nPerm) = let
                val () = for (0, n-1, fn i => A.update(perm, i, A.sub(perm1, i)))
                val r = let
                      fun lp 1 = 1
                        | lp r = (A.update (count, r-1, r); lp(r-1))
                      in
                        lp r
                      end
                val nFlips = countFlips perm
                val maxFlips = Int.max(maxFlips, nFlips)
                val checkSum = if Word.andb(Word.fromInt nPerm, 0w1) = 0w0
                      then checkSum + nFlips
                      else checkSum - nFlips
                fun lp r = if (r = n) then 0
                      else let
                        val t = A.sub(perm1, 0)
                        in
                          for (0, r-1, fn i => A.update(perm1, i, A.sub(perm1, i+1)));
                          A.update(perm1, r, t);
                          A.update(count, r, A.sub(count, r) - 1);
                          if A.sub(count, r) > 0
                            then r
                            else lp (r + 1)
                        end
                in
                  loop (lp r, maxFlips, checkSum, nPerm+1)
                end
          val (maxFlips, checkSum) = loop (n, 0, 0, 0)
          in
            Log.say [
                Int.toString checkSum,
                "\nPfannkuchen(", Int.toString n, ") = ", Int.toString maxFlips, "\n"
              ]
          end

    fun testit () = fannkuch 7

    fun doit () = (fannkuch 11; fannkuch 11; fannkuch 11)

  end
