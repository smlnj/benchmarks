(* time.sml
 *
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * time = lab^k
 *
 * In k-CFA, time is a bounded memory of program history.
 * In particular, it is the last k call sites through which
 * the program has traversed.
*)

structure Time =
  struct

    type t = Label.t list

    val compare = List.collate Label.compare
    val same = ListPair.allEq Label.same

    val zero = []

  end
