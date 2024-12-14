(* camera.sml
 *
 * COPYRIGHT (c) 2019 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure Camera : sig

    type t

    val make : {
	    wid : int, ht : int, ns : int,
	    pos : Vec3.t,
            lookAt : Vec3.t,
            up : Vec3.t,
            fov : Real64.real
	  } -> t

  (* simple camera located at the origin looking down the negative Z axis *)
    val simpleCamera : {
	    wid : int, ht : int, ns : int, flen : Real64.real
	  } -> t

    type pixel_renderer = (int * int -> Color.t)

    val makePixelRenderer : (int * int -> RGB.t) * (RGB.t -> Color.t) -> pixel_renderer

    val foreachPixel : t * pixel_renderer -> Image.t

    val pixelToRGB : t * (Ray.t -> RGB.t) -> int * int -> RGB.t

    val rayToRGB : Ray.t -> RGB.t

    val aaPixelToRGB : t * (Ray.t -> RGB.t) -> int * int -> RGB.t

  end = struct

    datatype t = Cam of {
	wid : int,	(* width of image *)
	ht : int,	(* height of image *)
	ns : int,	(* number of samples per pixel *)
	pos : Vec3.t,	(* position of camera *)
	ulc : Vec3.t,	(* upper-left-corner of image plane *)
	hvec : Vec3.t,	(* horizontal pixel-wide vector parallel to image pointing right *)
	vvec : Vec3.t	(* vertical pixel-wide vector parallel to image pointing down *)
      }

    fun make {wid, ht, ns, pos, lookAt, up, fov} = let
	  val dir = Vec3.normalize (Vec3.sub (lookAt, pos))
	  val right = Vec3.normalize (Vec3.cross (dir, up))
	  val up = Vec3.normalize (Vec3.cross (right, dir))
	  val pw = 2.0 / Real64.fromInt wid
	  val aspect = Real64.fromInt ht / Real64.fromInt wid
	  val theta = (Real64.Math.pi * fov) / 180.0
	  val flen = 1.0 / Real64.Math.tan (0.5 * theta)
	  val imgCenter = Vec3.add(pos, Vec3.scale(flen, dir))
	  val ulc = Vec3.sub(Vec3.add(imgCenter, Vec3.scale(aspect, up)), right)
	  in
	    Cam{
		wid = wid, ht = ht, ns = ns,
		pos = pos,
		ulc = ulc,
		hvec = Vec3.scale(pw, right),
		vvec = Vec3.scale(~pw, up)
	      }
	  end

    fun simpleCamera {wid, ht, ns, flen} = let
	  val pw = 2.0 / Real64.fromInt wid
	  val aspect = Real64.fromInt ht / Real64.fromInt wid
	  in
	    Cam{
		wid = wid, ht = ht, ns = ns,
		pos = Vec3.zero,
		ulc = (0.5 * pw - 1.0, aspect, ~ flen),
		hvec = (pw, 0.0, 0.0),
		vvec = (0.0, ~pw, 0.0)
	      }
	  end

    type pixel_renderer = (int * int -> Color.t)

    fun makePixelRenderer (toRGB, cvt) coords = cvt(toRGB coords)

    fun foreachPixel (Cam{wid, ht, ...}, pr) = let
	  fun rowLp (r, colors) = if (r < 0) then colors else colLp(r-1, wid-1, colors)
	  and colLp (r, c, colors) = if (c < 0)
		then rowLp (r, colors)
		else colLp (r, c-1, pr(r, c) :: colors)
	  in
	    Image.Img{
		wid = wid, ht = ht,
		pixels = rowLp (ht - 1, [])
	      }
	  end

    fun rayForPixel (Cam{pos, ulc, hvec, vvec, ...}) = let
	  val ulcCenter = Vec3.adds(ulc, 0.5, Vec3.add(hvec, vvec))
	  in
	    fn (r, c) => Ray.make (
		pos,
		Vec3.add (ulcCenter,
		  Vec3.add (
                    Vec3.scale (Real64.fromInt r, vvec),
		    Vec3.scale (Real64.fromInt c, hvec))))
	  end

    fun pixelToRGB (cam, trace) = let
	  val rfp = rayForPixel cam
	  in
	    fn coords => trace (rfp coords)
	  end

    fun rayToRGB ((_, (_, y, _)) : Ray.t) =
	  RGB.lerp (RGB.white, 0.5 * (y + 1.0), (0.5, 0.7, 1.0))

    fun raysForPixel (Cam{ns, pos, ulc, hvec, vvec, ...}) (r, c) = let
	  val r = Real64.fromInt r
	  val c = Real64.fromInt c
	  val ulcDir = Vec3.sub(ulc, pos)
	  fun mkRay _ = let
		val dir = Vec3.adds(ulcDir, r + Rand.rand(), vvec)
		val dir = Vec3.adds(dir, c + Rand.rand(), hvec)
		in
		  Ray.make (pos, dir)
		end
	  in
	    List.tabulate (ns, mkRay)
	  end

    fun aaPixelToRGB (cam as Cam{ns, ...}, trace) = let
	  val rfp = raysForPixel cam
	  val scale = if ns = 0 then 1.0 else 1.0 / Real64.fromInt ns
	  in
	    fn coords => RGB.scale(
		scale,
		List.foldl
		  (fn (ray, c) => RGB.add(c, trace ray))
		    RGB.black (rfp coords))
	  end

  end
