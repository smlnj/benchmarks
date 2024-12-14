(* ray.sml
 *
 * COPYRIGHT (c) 2019 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure Ray : sig

    type t = Vec3.t * Vec3.t

    val make : Vec3.t * Vec3.t -> t

    val eval : t * Real64.real -> Vec3.t

  end = struct

    type t = (Vec3.t * Vec3.t)

    fun make (origin, dir) = (origin, Vec3.normalize dir)

    fun eval (r : t, t) = Vec3.adds (#1 r, t, #2 r)

  end
