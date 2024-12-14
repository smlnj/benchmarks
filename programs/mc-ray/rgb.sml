(* rgb.sml
 *
 * COPYRIGHT (c) 2019 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure RGB : sig

    type t = (Real64.real * Real64.real * Real64.real)

    val add : (t * t) -> t
    val adds : (t * Real64.real * t) -> t
    val modulate : (t * t) -> t

    val scale : (Real64.real * t) -> t

  (* lerp (u, t, v) = (1-t)*u + t*v; we assume that 0 <= t <= 1 *)
    val lerp : (t * Real64.real * t) -> t

  (* standard colors *)
    val black : t
    val red : t
    val green : t
    val blue : t
    val white : t
    val gray : Real64.real -> t

  end = struct

    type t = (Real64.real * Real64.real * Real64.real)

    fun add ((r1, g1, b1) : t, (r2, g2, b2)) = (r1 + r2, g1 + g2, b1 + b2)
    fun adds ((r1, g1, b1) : t, s, (r2, g2, b2)) = (r1 + s*r2, g1 + s*g2, b1 + s*b2)
    fun modulate ((r1, g1, b1) : t, (r2, g2, b2)) = (r1 * r2, g1 * g2, b1 * b2)

    fun scale (s, (r, g, b) : t) = (s*r, s*g, s*b)

    fun lerp (c1, t, c2) = add (scale(1.0 - t, c1), scale(t, c2))

  (* standard colors *)
    val black : t = (0.0, 0.0, 0.0)
    val red : t = (1.0, 0.0, 0.0)
    val green : t = (0.0, 1.0, 0.0)
    val blue : t = (0.0, 0.0, 1.0)
    val white : t = (1.0, 1.0, 1.0)
    fun gray v = (v, v, v)

  end

