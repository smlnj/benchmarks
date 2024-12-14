(* vec3-sig.sml
 *
 * COPYRIGHT (c) 2012 The SML3d Project (http://sml3d.cs.uchicago.edu)
 * All rights reserved.
 *)

signature VEC3 =
  sig

    type t = (Real64.real * Real64.real * Real64.real)

    val toString : t -> string

  (* zero vector *)
    val zero : t

  (* vector arithmetic *)
    val negate : t -> t
    val add : (t * t) -> t
    val sub : (t * t) -> t
    val mul : (t * t) -> t

    val scale : (Real64.real * t) -> t

  (* adds (u, s, v) = u + s*v *)
    val adds : (t * Real64.real * t) -> t

  (* lerp (u, t, v) = (1-t)*u + t*v; we assume that 0 <= t <= 1 *)
    val lerp : (t * Real64.real * t) -> t

    val dot : (t * t) -> Real64.real

    val normalize : t -> t

    val length : t -> Real64.real

  (* cross product *)
    val cross : t * t -> t

  (* return the parallel and perpendicular components of a vector v
   * relative to a unit basis vector.
   *)
    val parallelComponent : {basis : t, v : t} -> t
    val perpendicularComponent : {basis : t, v : t} -> t

  (* iterators *)
    val app  : (Real64.real -> unit) -> t -> unit
    val map  : (Real64.real -> 'a) -> t -> ('a * 'a * 'a)

  (* graphics related functions *)

    val reflect : {v : t, n : t} -> t

    val rotateX : Real64.real -> t -> t
    val rotateY : Real64.real -> t -> t
    val rotateZ : Real64.real -> t -> t

    val randomPointInSphere : unit -> t

  end
