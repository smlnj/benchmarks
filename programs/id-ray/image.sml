(* image.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure Image : sig

    type t

    val new : int * int -> t
    val update : t * int * int * real * real * real -> unit
    val output : t * string -> unit

  end = struct

    structure BinIO = Log.BinIO

    datatype t = IMG of {
        wid : int,
        ht : int,
        data : Word8Array.array
      }

    fun clamp d = if d < 0.0 then 0.0 else if d > 1.0 then 1.0 else d

    fun new (w, h) = IMG{
            wid = w, ht = h,
            data = Word8Array.array(3 * w * h, 0w0)
          }

    fun update (IMG{wid, ht, data}, row, col, r, g, b) = let
  	  fun cvt x = Word8.fromInt(Real.round((clamp x) * 255.0))
          val idx = wid * row + col
          in
            Word8Array.update(data, idx+0, cvt r);
            Word8Array.update(data, idx+1, cvt g);
            Word8Array.update(data, idx+2, cvt b)
          end

    fun output (IMG{wid, ht, data}, outFile) = let
          val outS = BinIO.openOut outFile
	  fun pr s = BinIO.output(outS, Byte.stringToBytes s)
          in
	    pr "P6\n";
	    pr (concat[Int.toString wid, " ", Int.toString ht, "\n"]);
	    pr "255\n";
(* NOTE: the SML Basis Library should provide an output function for arrays! *)
            Word8Array.app (fn b => BinIO.output1(outS, b)) data;
            BinIO.closeOut outS
          end

  end
