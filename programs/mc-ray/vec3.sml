(* vec3.sml
 *
 * COPYRIGHT (c) 2012 The SML3d Project (http://sml3d.cs.uchicago.edu)
 * All rights reserved.
 *
 * Operations on vectors in R^3 (scalar version)
 *)

structure Vec3 : VEC3 =
  struct

    val epsilon = 0.0001

    type t = (Real64.real * Real64.real * Real64.real)

    fun toString ((x, y, z) : t) = concat[
	    "<", Real64.toString x, ",", Real64.toString y, ",", Real64.toString z, ">"
	  ]

    val zero : t = (0.0, 0.0, 0.0)

    val e1 : t = (1.0, 0.0, 0.0)
    val e2 : t = (0.0, 1.0, 0.0)
    val e3 : t = (0.0, 0.0, 1.0)

    fun negate ((x, y, z) : t) = (~x, ~y, ~z)

    fun add ((x1, y1, z1) : t, (x2, y2, z2)) = (x1+x2, y1+y2, z1+z2)

    fun sub ((x1, y1, z1) : t, (x2, y2, z2)) = (x1-x2, y1-y2, z1-z2)

    fun mul ((x1, y1, z1) : t, (x2, y2, z2)) = (x1*x2, y1*y2, z1*z2)

    fun scale (s, (x, y, z) : t) = (s*x, s*y, s*z)

    fun adds (v1, s, v2) = add(v1, scale(s, v2))

    fun dot ((x1, y1, z1) : t, (x2, y2, z2)) = (x1*x2 + y1*y2 +z1*z2)

    fun lerp (v1, t, v2) = adds (scale(1.0 - t, v1), t, v2)

    fun cross ((x1, y1, z1) : t, (x2, y2, z2)) = (
	    y1*z2 - z1*y2,
	    z1*x2 - x1*z2,
	    x1*y2 - y1*x2
	  )

    fun lengthSq v = dot(v, v)
    fun length v = Real64.Math.sqrt(lengthSq v)

    fun distanceSq (u, v) = lengthSq (sub (u, v))
    fun distance (u, v) = length (sub (u, v))

    fun lengthAndDir v = let
	  val l = length v
	  in
	    if (l < epsilon)
	      then (0.0, zero)
	      else (l, scale(1.0 / l, v))
	  end

    fun normalize v = #2(lengthAndDir v)

    fun parallelComponent {basis, v} = scale(dot(basis, v), basis)

    fun perpendicularComponent {basis, v} = sub(v, parallelComponent {basis=basis, v=v})

    fun reflect {v : t, n : t} = adds (v, ~2.0 * dot(v, n), n)

    fun rotateX angle = let
	  val theta = (Real64.Math.pi * angle) / 180.0
	  val s = Real64.Math.sin theta
	  val c = Real64.Math.cos theta
	  in
	    fn ((x, y, z) : t) => (x, c * y - s * z, s * y + c * z)
	  end

    fun rotateY angle = let
	  val theta = (Real64.Math.pi * angle) / 180.0
	  val s = Real64.Math.sin theta
	  val c = Real64.Math.cos theta
	  in
	    fn ((x, y, z) : t) => (c * x + s * z, y, c * z - s * x)
	  end

    fun rotateZ angle = let
	  val theta = (Real64.Math.pi * angle) / 180.0
	  val s = Real64.Math.sin theta
	  val c = Real64.Math.cos theta
	  in
	    fn ((x, y, z) : t) => (c * x - s * y, s * x + c * y, z)
	  end

    fun randomPointInSphere () = let
	  val pt = (Rand.rand(), Rand.rand(), Rand.rand())
	  in
	    if (dot(pt, pt) < 1.0) then pt else randomPointInSphere()
	  end

  (* iterators *)
    fun map f (x, y, z) = (f x, f y, f z)
    fun app (f : 'a -> unit) (x, y, z) = (f x; f y; f z)

  end
