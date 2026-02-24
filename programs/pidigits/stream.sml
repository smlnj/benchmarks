(* stream.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure Stream =
  struct

    datatype 'a u = Nil | Cons of 'a * 'a t
    withtype 'a t = unit -> 'a u

    fun unfold (f : 'b -> ('a * 'b) option) : 'b -> 'a t = let
          fun loop b () = (case f b
                of NONE => Nil
                 | SOME (x, b) => Cons (x, loop b))
          in
            loop
          end

    fun map (f : 'a -> 'b) : 'a t -> 'b t =
          unfold (fn s => case s () of Nil => NONE | Cons (x, xs) => SOME (f x, xs))

  end
