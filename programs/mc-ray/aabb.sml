(* aabb.sml
 *
 * COPYRIGHT (c) 2019 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure AABB : sig

    datatype t = BB of {
	min : Vec3.t,
	max : Vec3.t
      }

    val toString : t -> string

    val hitTest : t * Ray.t * Interval.t -> bool

    val union : t * t -> t

  end = struct

    datatype t = BB of {
	min : Vec3.t,
	max : Vec3.t
      }

    fun toString (BB{min, max}) = String.concat[
	    "(", Vec3.toString min, ", ", Vec3.toString max, ")"
	  ]

  (* fast min/max functions for reals *)
    fun fmin (x : Real64.real, y) = if (x < y) then x else y
    fun fmax (x : Real64.real, y) = if (x > y) then x else y

    fun hitTest (BB{min, max}, (ro, rd) : Ray.t, minMaxT : Interval.t) = let
	  fun chk (minW, maxW, roW, rdW, (minT, maxT)) = let
		fun chk (t0, t1) = let
		      val minT = fmax(t0, minT)
		      val maxT = fmin(t1, maxT)
		      in
			if (maxT <= minT) then NONE else SOME(minT, maxT)
		      end
		val invD = 1.0 / rdW
		val t0 = (minW - roW) * invD
		val t1 = (maxW - roW) * invD
 		in
		  if (invD < 0.0) then chk(t1, t0) else chk(t0, t1)
		end
	  in
	    case chk (#1 min, #1 max, #1 ro, #1 rd, minMaxT)
	     of SOME minMaxT => (case chk (#2 min, #2 max, #2 ro, #2 rd, minMaxT)
		   of SOME minMaxT => (case chk (#3 min, #3 max, #3 ro, #3 rd, minMaxT)
			 of SOME _ => true
			  | NONE => false
			(* end case *))
		    | NONE => false
		  (* end case *))
	      | NONE => false
	    (* end case *)
	  end

    fun union (BB{min=min1, max=max1}, BB{min=min2, max=max2}) =
	  BB{
	      min = (fmin(#1 min1, #1 min2), fmin(#2 min1, #2 min2), fmin(#3 min1, #3 min2)),
	      max = (fmax(#1 max1, #1 max2), fmax(#2 max1, #2 max2), fmax(#3 max1, #3 max2))
	    }

  end
