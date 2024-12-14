(* trace.sml
 *
 * COPYRIGHT (c) 2019 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure Trace : sig

  (* ray caster for testing purposes *)
    val castRay : Object.t -> Ray.t -> RGB.t

  (* Given a world object and a maximum tracing depth, this function
   * returns a function that will recursively trace a ray through the
   * world to compute a color
   *)
    val traceRay : Object.t * int -> Ray.t -> RGB.t

    val rayTracer : Camera.t * Object.t -> Image.t

  end = struct

    fun castRay world ray = (
	  case Object.hitTest (world, ray, (0.0, Real64.posInf))
	   of Object.Miss => Camera.rayToRGB ray
	    | Object.Hit hit => (case Material.getHitInfo(hit, ray)
		 of NONE => Material.getEmission hit
		  | SOME{aten, ...} => RGB.add(Material.getEmission hit, aten)
		(* end case *))
	  (* end case *))

    fun traceRay (world, maxDepth) = let
	  val minMaxT = (0.001, Real64.posInf)
	  fun trace (ray, depth) = if (depth <= 0)
		then RGB.black
		else (case Object.hitTest (world, ray, minMaxT)
		   of Object.Miss => Camera.rayToRGB ray
		    | Object.Hit hit => (case Material.getHitInfo(hit, ray)
			 of NONE => Material.getEmission hit
			  | SOME{aten, reflect} => RGB.add(
			      Material.getEmission hit,
			      RGB.modulate(aten, trace (reflect, depth-1)))
			(* end case *))
		  (* end case *))
	  in
	    fn ray => trace(ray, maxDepth)
	  end

    fun rayTracer (cam, world) =
	  Camera.foreachPixel (
	    cam,
	    Camera.makePixelRenderer (
	      Camera.aaPixelToRGB(cam, traceRay (world, 20)),
	      Color.fromRGBWithGamma))

  end
