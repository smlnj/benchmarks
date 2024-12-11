(* caml.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure Caml =
struct

exception Not_found

fun failwith s = raise(Fail s)

local
   open TextIO
in
   type out_channel = outstream
   val open_out = openOut
   val open_out_bin = open_out
   fun output_string (out, s) = output(out, s)
   val close_out = closeOut
end

structure Array =
   struct
      local open Array
      in
         val array = array
         val copy = copy
         val of_list = fromList
         val length = length
         val sub = sub
         val update = update
         val unsafe_get = Unsafe.Array.sub
         val unsafe_set = Unsafe.Array.update
         val make = array
         fun map f a = Array.tabulate(length a, fn i => f(Array.sub(a, i)))
         val init = tabulate
      end
   end

fun for(a: int, b, f) =
   let
      fun loop a =
         if a > b
            then ()
         else (f a; loop(a + 1))
   in loop a
   end

fun forDown(b: int, a, f) =
   let
      fun loop b =
         if b < a
            then ()
         else (f b; loop(b - 1))
   in loop b
   end

(* A hack for hash tables with string domain where performance doesn't matter. *)
structure Hashtbl:
   sig
      type ('a, 'b) t

      val add: ('a, 'b) t -> string -> 'b -> unit
      val create: int -> ('a, 'b) t
      val find: ('a, 'b) t -> string -> 'b
   end =
   struct
      datatype ('a, 'b) t = T of (string * 'b) list ref

      fun create _ = T (ref [])

      fun add (T t) k d = t := (k, d) :: !t

      fun find (T (ref t)) k =
         case List.find (fn (k', _) => k = k') t of
            NONE => raise Not_found
          | SOME(_, d) => d
   end

fun exit i = Posix.Process.exit(Word8.fromInt i)

end
