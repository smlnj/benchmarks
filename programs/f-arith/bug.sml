(* main.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure Main =
  struct

(*
    fun computePi steps = let
          fun loop (0, acc, _) = acc
            | loop (steps, acc, n) = let
                val acc' = acc + (1.0 / n) - (1.0 / (n + 2.0))
                in
                  loop (steps - 1, acc', n + 4.0)
                end
          in
            4.0 * loop (steps, 0.0, 1.0)
          end

    fun run n = let
          val pi = computePi n
          in
            print (concat [
                "This should be an approximation of pi: ",
                Real.toString pi, " (", Int.toString n, " steps)\n"
              ]);
            n
          end
*)
    fun run (n : int) = n

  end;

(*
val _ = Main.run 5000000000
*)