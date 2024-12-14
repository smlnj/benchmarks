(* material.sml
 *
 * COPYRIGHT (c) 2019 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure Material : sig

    type t

    datatype hit = Hit of {
        t : Real64.real,
        pt : Vec3.t,
        norm : Vec3.t,
        material : t
      }

    val getEmission : hit -> RGB.t
    val getHitInfo : hit * Ray.t -> {aten : RGB.t, reflect : Ray.t} option

    val flat : RGB.t -> t
    val normal : t
    val lambertian : RGB.t -> t
    val metal : RGB.t * Real64.real -> t
    val diffuseLight : RGB.t -> t

  end = struct

    datatype hit = Hit of {
        t : Real64.real,
        pt : Vec3.t,
        norm : Vec3.t,
        material : t
      }

    and t = Material of {
        emit : hit -> RGB.t,
        scatter : Ray.t * hit -> {aten : RGB.t, reflect : Ray.t} option
      }

    fun getEmission (hit as Hit{material=Material{emit, ...}, ...}) = emit hit

    fun getHitInfo (hit as Hit{material=Material{scatter, ...}, ...}, ray) =
	  scatter (ray, hit)

    fun flat rgb = Material{
	    emit = fn _ => RGB.black,
	    scatter = fn _ => SOME{aten=rgb, reflect=(Vec3.zero, Vec3.zero)}
	  }

    val normal = Material{
	    emit = fn _ => RGB.black,
	    scatter = fn (_, Hit{norm=(x, y, z), ...}) => SOME{
		aten = (0.5 * (x + 1.0), 0.5 * (y + 1.0), 0.5 * (z + 1.0)),
		reflect = (Vec3.zero, Vec3.zero)
	      }
	  }

    fun lambertian albedo = Material{
	    emit = fn _ => RGB.black,
	    scatter = fn (ray, Hit{pt, norm, ...}) => SOME{
		  aten = albedo,
		  reflect = Ray.make(pt, Vec3.add(norm, Vec3.randomPointInSphere()))
		}
	  }

    fun metal (albedo, fuzz) = Material{
	    emit = fn _ => RGB.black,
	    scatter = fn ((_, rdir), Hit{pt, norm, ...}) => let
		val dir = Vec3.adds(
		      Vec3.reflect{v = rdir, n = norm},
		      fuzz,
		      Vec3.randomPointInSphere())
		in
		  if Vec3.dot(dir, norm) <= 0.0
		    then NONE
		    else SOME{
			aten = albedo,
			reflect = Ray.make(pt, dir)
		      }
		end
	  }

    fun diffuseLight rgb = Material{
	    emit = fn _ => rgb,
	    scatter = fn _ => NONE
	  }

  end

