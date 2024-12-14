(* image.sml
 *
 * COPYRIGHT (c) 2019 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure Image : sig

    datatype t = Img of {
	wid : int,
	ht : int,
	pixels : Color.t list
      }

    val writePPM : string * t -> unit

  end = struct

    datatype t = Img of {
	wid : int,
	ht : int,
	pixels : Color.t list
      }

    fun writePPM (file, Img{wid, ht, pixels}) = let
	  val outS = BinIO.openOut file
	  fun pr s = BinIO.output(outS, Byte.stringToBytes s)
	  fun out1 b = BinIO.output1(outS, b)
	  in
	  (* write header *)
	    pr "P6\n";
	    pr (concat[Int.toString wid, " ", Int.toString ht, "\n"]);
	    pr "255\n";
	  (* write pixels *)
	    List.app (fn (r, g, b) => (out1 r; out1 g; out1 b)) pixels;
	  (* close file *)
	    BinIO.closeOut outS
	  end

  end
