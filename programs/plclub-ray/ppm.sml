(* ppm.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

signature PPM =
   sig
      type pixmap

      val init : (*width:*)int * (*height:*)int -> pixmap
      val dump : string * pixmap -> unit
(*      val load : string -> pixmap *)

      val width : pixmap -> int
      val height : pixmap -> int

      val get : pixmap * int * int * int -> int
      val set : pixmap * int * int * int * int -> unit
      val setp : pixmap * int * int * int * int * int -> unit
   end
structure Ppm: PPM =
struct

open Caml

structure Array = Word8Array
structure Word = Word8

type pixmap = Array.array * int

fun get ((img, width), i, j, k) =
   Word.toInt (Array.sub (img, ((j * width) + i) * 3 + k))

fun set ((img, width), i, j, k, v) =
   Array.update (img, ((j * width) + i) * 3 + k, Word.fromInt v)

fun setp ((img, width), i, j, r, g, b) =
  let val p = ((j * width) + i) * 3
  in Array.update(img, p, Word.fromInt r)
     ; Array.update(img, p + 1, Word.fromInt g)
     ; Array.update(img, p + 2, Word.fromInt b)
  end

fun init (width, height) =
   (Array.array(height * width * 3, 0w0), width)

fun width (s, width) = width
fun height (s, width) = Array.length s div width div 3

fun dump (file, (img, width)) =
  let
     val sz = Array.length img
     val height = sz div 3 div width
     val f = open_out_bin file
  in output_string (f, "P6\n# PL Club - translated to SML\n")
     ; output_string (f, concat[Int.toString width, " ",
                               Int.toString height, "\n255\n"])
     ; output_string (f, Byte.unpackString (Word8ArraySlice.slice
                                            (img, 0, NONE)))
     ; close_out f
  end

(* fun load file =
 *   let f = open_in_bin file in
 *   assert (input_line f = "P6");
 *   assert ((input_line f).[0] = '#');
 *   let s = input_line f in
 *   let i = ref 0 in
 *   while s.[!i] >= '0' && s.[!i] <= '9' do incr i done;
 *   let width = int_of_string (String.sub s 0 !i) in
 *   let height =
 *     int_of_string (String.sub s (!i + 1) (String.length s - !i - 1)) in
 *   assert (input_line f = "255");
 *   let (s, _) as img = init width height in
 *   really_input f s 0 (String.length s);
 *   close_in f;
 *   img
 *)
end
