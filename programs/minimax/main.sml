(* main.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure Main : BMARK =
  struct

    structure T = TicTacToe

    val name = "minimax"

    val results : string list = []

    val iters = 10

    fun doit () = let
	    fun doit () = (
                  T.gameTree();
                  T.gameTreeWithTT())
            fun loop n = if n = 0
                  then ()
                  else (doit () ; loop (n-1))
            in
              loop iters
            end

    (* print the size of the tree *)
    fun printTree tr = let
          fun sz (T.Rose(_, kids)) = let
                fun doKid (t, (n, d)) = let
                      val (n', d') = sz t
                      in
                        (n+n', Int.max(d, d'))
                      end
                val (n, d) = List.foldl doKid (0, 0) kids
                in
                  (n+1, d+1)
                end
          val (n, d) = sz tr
          in
            Log.say [
                "# nodes = ", Int.toString n, ", max depth = ", Int.toString d, "\n"
              ]
          end

    fun testit () = (
          Log.print "Minimax: ";
          printTree (T.gameTree());
          Log.print "Minimax+TT: ";
          printTree (T.gameTreeWithTT()))

  end
