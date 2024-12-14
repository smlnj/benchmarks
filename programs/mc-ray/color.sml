(* color.sml
 *
 * COPYRIGHT (c) 2019 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure Color : sig

    type t = Word8.word * Word8.word * Word8.word

    val fromRGB : RGB.t -> t

  (* convert an RGB value to an image color value with a gamma correction of 1/2 *)
    val fromRGBWithGamma : RGB.t -> t

  end = struct

    type t = Word8.word * Word8.word * Word8.word

    fun toByte (f : Real64.real) = let
	  val f' = Real64.floor (f * 255.99)
	  in
	    if (f' <= 0) then 0w0
	    else if (255 <= f') then 0w255
	    else Word8.fromInt f'
	  end

    fun fromRGB ((r, g, b) : RGB.t) = (toByte r, toByte g, toByte b)

    fun fromRGBWithGamma ((r, g, b) : RGB.t) = let
	  fun cvt f = toByte (Real64.Math.sqrt f)
	  in
	    (cvt r, cvt g, cvt b)
	  end

  end
