(* streams.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure Streams : sig

    type 'a t

    val make : 'a * (unit -> 'a t) -> 'a t

    val unfold : 'a t -> 'a * 'a t

    val get : 'a t * int -> 'a

    val take : 'a t * int -> 'a list

  end = struct

    datatype 'a t = S of 'a * (unit -> 'a t)

    val make = S

    fun unfold (S(fst, thunk)) = (fst, thunk())

    fun get (strm, n) = if (n < 0)
          then raise Subscript
          else let
            fun lp (0, S(fst, _)) = fst
              | lp (n, S(_, thunk)) = lp (n-1, thunk())
            in
              lp (n, strm)
            end

    fun take (strm, n) = if (n < 0)
          then raise Subscript
          else let
            fun lp (0, _) = []
              | lp (n, S(fst, thunk)) = fst :: lp(n-1, thunk())
            in
              lp (n, strm)
            end

  end
