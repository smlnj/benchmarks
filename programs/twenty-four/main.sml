(* main.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure Main : BMARK =
  struct

    val name = "twenty-four"

    val results : string list = []

    fun allHands curr left =
        if left < 0 orelse left > curr
          then []
        else if curr = 0
          then [[]]
          else List.@ (
            List.map (fn y => curr :: y) (allHands (curr - 1) (left - 1)),
            allHands (curr - 1) left)

    fun countSolutions hand =
          TwentyFour.solve hand (fn (_, resume) => 1 + resume ()) (fn () => 0)

    fun testit () = if (countSolutions [4, 7, 8, 8] = 44)
          then Log.print "OK\n"
          else Log.print "FAIL"

    fun doit () = let
          val nSoln = ref 0
          val hands = allHands 10 4
          fun runHand hand = (nSoln := !nSoln + countSolutions hand)
          fun lp 0 = ()
            | lp n = (List.app runHand hands; lp (n - 1))
          in
            lp 250
          end

  end
