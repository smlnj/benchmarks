(* pi-digits.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure PiDigits : sig

    val pi : IntInf.int Stream.t

  end = struct

    fun stream {
            next : 'b -> 'c,
            safe : 'b -> 'c -> bool,
            prod : 'b -> 'c -> 'b,
            cons : 'b -> 'a -> 'b
          } : 'b -> 'a Stream.t -> 'c Stream.t = let
          fun loop z s () = let
                val y = next z
                in
                  if safe z y
                    then Stream.Cons (y, loop (prod z y) s)
                    else (case s ()
                       of Stream.Nil => Stream.Nil
                        | Stream.Cons (x, xs) => loop (cons z x) xs ())
                end
          in
            loop
          end

    type lft = (IntInf.int * IntInf.int * IntInf.int * IntInf.int)

    val unit : lft = (1,0,0,1)

    fun comp (q,r,s,t) (u,v,w,x) : lft = (q*u+r*w, q*v+r*x, s*u+t*w, s*v+t*x)

    val pi = let
          val init = unit
          val lfts = Stream.map (fn k => (k, 4*k+2, 0, 2*k+1)) (Stream.unfold (fn i => SOME (i, i+1)) 1)
          fun floor_extr (q,r,s,t) x = (q * x + r) div (s * x + t)
          fun next z = floor_extr z 3
          fun safe z n = n = floor_extr z 4
          fun prod z n = comp (10, ~10*n, 0, 1) z
          fun cons z z' = comp z z'
          in
            stream {next = next, safe = safe, prod = prod, cons = cons} init lfts
          end

  end
