(* sphere.sml
 *
 * COPYRIGHT (c) 2019 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure Sphere : sig

    val make : Vec3.t * Real64.real * Material.t -> Object.t

  end = struct

    fun make (center, radius, material) = let
	  val rSq = radius * radius
	  val invR = 1.0 / radius
	  fun hitTest (ray as (ro, rd), minMaxT) = let
		val q = Vec3.sub(ro, center)
		val b = 2.0 * Vec3.dot(rd, q)
		val c = Vec3.dot(q, q) - rSq
		val disc = b*b - 4.0*c
		in
		  if (disc < 0.0)
		    then Object.Miss
		    else let
		      val t = 0.5 * (~b - Real64.Math.sqrt disc)
		      in
			if Interval.within(t, minMaxT)
			  then let
			    val pt = Ray.eval(ray, t)
			    in
			      Object.Hit(Material.Hit{
				  t = t, pt = pt,
				  norm = Vec3.scale(invR, Vec3.sub(pt, center)),
				  material = material
				})
			    end
			  else Object.Miss
		      end
		end
	  in
	    Object.Obj{
		hit = hitTest,
		bbox = fn () => SOME(AABB.BB{
		    min = Vec3.sub(center, (radius, radius, radius)),
		    max = Vec3.add(center, (radius, radius, radius))
		  })
	      }
	  end

  end
