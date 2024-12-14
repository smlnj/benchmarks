(* object.sml
 *
 * COPYRIGHT (c) 2019 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure Object : sig

    datatype maybe_hit = Miss | Hit of Material.hit

    datatype t = Obj of {
	hit : Ray.t * Interval.t -> maybe_hit,
	bbox : unit -> AABB.t option
      }

  (* test a ray against an object *)
    val hitTest : t * Ray.t * Interval.t -> maybe_hit

  (* get an object's bounding box *)
    val boundingBox : t -> AABB.t option

  (* an empty object that cannot be hit by rays *)
    val empty : t

  (* make an object from a list of objects *)
    val fromList : t list -> t

  (* translate the object by the given offset *)
    val translate : Vec3.t * t -> t

  (* rotate the object counter-clockwise by the specified angle (in degrees) *)
    val rotateX : Real64.real * t -> t
    val rotateY : Real64.real * t -> t
    val rotateZ : Real64.real * t -> t

  end = struct

    datatype maybe_hit = Miss | Hit of Material.hit

    datatype t = Obj of {
	hit : Ray.t * Interval.t -> maybe_hit,
	bbox : unit -> AABB.t option
      }

    fun hitTest (Obj{hit, ...}, ray, minMaxT) = hit(ray, minMaxT)

    fun boundingBox (Obj{bbox, ...}) = bbox()

    val empty = Obj{hit = fn _ => Miss, bbox = fn () => NONE}

  (* fast min/max functions for reals *)
    fun fmin (x : Real64.real, y) = if (x < y) then x else y
    fun fmax (x : Real64.real, y) = if (x > y) then x else y

    fun closer (Miss, maybeHit) = maybeHit
      | closer (maybeHit, Miss) = maybeHit
      | closer (
	    hit1 as Hit(Material.Hit{t=t1, ...}),
	    hit2 as Hit(Material.Hit{t=t2, ...})
	  ) = if (t1 <= t2) then hit1 else hit2

    fun fromList [] = empty
      | fromList [obj] = obj
      | fromList (objs as obj1::objr) = let
	  fun hitTest' (ray, minMaxT) = List.foldl
		(fn (obj, mhit) => closer(mhit, hitTest(obj, ray, minMaxT)))
		  Miss objs
	  fun bbox' () = let
		fun grow ([], bb) = SOME bb
		  | grow (obj::objr, bb) = (case boundingBox obj
		       of NONE => NONE
			| SOME bb' => grow (objr, AABB.union(bb, bb'))
		      (* end case *))
		in
		  case boundingBox obj1
		   of NONE => NONE
		    | SOME bb => grow(objr, bb)
		  (* end case *)
		end
	  in
	    Obj{hit = hitTest', bbox = bbox'}
	  end

    fun translate (delta, Obj{hit, bbox}) = let
	  fun hitTest' ((origin, dir), minMaxT) = (
		case hit ((Vec3.sub(origin, delta), dir), minMaxT)
	         of Hit(Material.Hit{t, pt, norm, material}) =>
		      Hit(Material.Hit{
			  t = t,
			  pt = Vec3.add(pt, delta),
			  norm = norm,
			  material = material
			})
		  | Miss => Miss
		(* end case *))
	  fun bbox' () = (case bbox()
		 of NONE => NONE
		  | SOME(AABB.BB{min, max}) => SOME(AABB.BB{
			min = Vec3.add(min, delta),
			max = Vec3.add(max, delta)
		      })
		(* end case *))
	  in
	    Obj{hit = hitTest', bbox = bbox'}
	  end

    fun rotateBB toObj bbox () = (case bbox()
	   of NONE => NONE
	    | SOME(AABB.BB{min=(x1, y1, z1), max=(x2, y2, z2)}) => let
		fun grow ([], x1, y1, z1, x2, y2, z2) =
		      SOME(AABB.BB{min=(x1, y1, z1), max=(x2, y2, z2)})
		  | grow (pt::ptr, x1, y1, z1, x2, y2, z2) = let
		      val (x, y, z) = toObj pt
		      in
			grow (
			  ptr,
			  fmin(x, x1), fmin(y, y1), fmin(z, z1),
			  fmax(x, x2), fmax(y, x2), fmax(z, z2))
		      end
		in
		  grow ([
		      (x1, y1, z1),
		      (x1, y1, z2),
		      (x1, y2, z1),
		      (x1, y2, z2),
		      (x2, y1, z1),
		      (x2, y1, z2),
		      (x2, y2, z1),
		      (x2, y2, z2)
		    ],
		    Real64.posInf, Real64.posInf, Real64.posInf,
		    Real64.negInf, Real64.negInf, Real64.negInf)
		end
	  (* end case *))

    fun rotateX (angle, Obj{hit, bbox}) = let
	  val toObj = Vec3.rotateX (~angle)
	  val toWorld = Vec3.rotateX angle
	  fun hitTest' ((origin, dir), minMaxT) = (
		case hit ((toObj origin, toObj dir), minMaxT)
		 of Hit(Material.Hit{t, pt, norm, material}) =>
		      Hit(Material.Hit{
			  t = t,
			  pt = toWorld pt,
			  norm = toWorld norm,
			  material = material
			})
		  | Miss => Miss
		(* end case *))
	  in
	    Obj{hit = hitTest', bbox = rotateBB toObj bbox}
	  end

    fun rotateY (angle, Obj{hit, bbox}) = let
	  val toObj = Vec3.rotateY (~angle)
	  val toWorld = Vec3.rotateY angle
	  fun hitTest' ((origin, dir), minMaxT) = (
		case hit ((toObj origin, toObj dir), minMaxT)
		 of Hit(Material.Hit{t, pt, norm, material}) =>
		      Hit(Material.Hit{
			  t = t,
			  pt = toWorld pt,
			  norm = toWorld norm,
			  material = material
			})
		  | Miss => Miss
		(* end case *))
	  in
	    Obj{hit = hitTest', bbox = rotateBB toObj bbox}
	  end

    fun rotateZ (angle, Obj{hit, bbox}) = let
	  val toObj = Vec3.rotateZ (~angle)
	  val toWorld = Vec3.rotateZ angle
	  fun hitTest' ((origin, dir), minMaxT) = (
		case hit ((toObj origin, toObj dir), minMaxT)
		 of Hit(Material.Hit{t, pt, norm, material}) =>
		      Hit(Material.Hit{
			  t = t,
			  pt = toWorld pt,
			  norm = toWorld norm,
			  material = material
			})
		  | Miss => Miss
		(* end case *))
	  in
	    Obj{hit = hitTest', bbox = rotateBB toObj bbox}
	  end

  end
