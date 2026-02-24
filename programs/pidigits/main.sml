(* main.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure Main : BMARK =
  struct

    val name = "pidigits"

    val results = []

    fun display n = let
          fun loop (ds, (k, col)) = if k < n
                then let
                  val col = if col = 10
                        then (Log.say["\t:", IntInf.toString k, "\n"]; 1)
                        else col + 1
                  in
                    case ds ()
                     of Stream.Nil => raise Empty
                      | Stream.Cons(d, ds) => (
                          Log.print (IntInf.toString d);
                          loop (ds, (k + 1, col)))
                  end
                else Log.say [
                    CharVector.tabulate (10 - col, fn _ => #" "),
                    "\t:", IntInf.toString k, "\n"
                  ]
          in
            loop (PiDigits.pi, (0, 0))
          end

    fun testit () = display 30

    fun doit () = display 2000

  end
