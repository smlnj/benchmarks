(* barnes-hut3d.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *
 * 3D version of the Barnes-Hut benchmark.
 *)

structure BarnesHut3D : sig

    structure S : SPACE
    structure V : VECTOR
    structure L : LOAD
    structure DataIO : DATA_IO

    (* reset the random number generator *)
    val srand : word -> unit

    (* generate the Plummer model data *)
    val testdata : int -> S.body list

    (* run the simulation *)
    val go : {
            output : {
                n2bcalc:int, nbccalc:int, nstep:int, selfint:int, tnow:real
              } -> unit,
	    bodies : S.body list, tnow : real, tstop : real,
	    dtime : real, eps : real, tol : real,
            rmin : real V.vec, rsize : real
	  } -> unit

  end = struct

    structure V = Vector3
    structure S = Space(V)
    structure L = Load(S)
    structure G = Grav(S)
    structure DataIO = DataIO(S)

    (* random numbers *)
    open Rand

    (* pick a random point on a sphere of specified radius. *)
    fun pickshell rad = let
          fun pickvec () = let
                val vec = V.tabulate (fn _ => xrand(~1.0, 1.0))
                val rsq = V.dotvp(vec, vec)
                in
                  if (rsq > 1.0)
                    then pickvec ()
                    else V.mulvs (vec, rad / Math.sqrt(rsq))
                end
          in
            pickvec ()
          end

    (* generate Plummer model initial conditions for test runs, scaled
     * to units such that M = -4E = G = 1 (Henon, Hegge, etc).
     * See Aarseth, SJ, Henon, M, & Wielen, R (1974) Astr & Ap, 37, 183.
     *)
    fun testdata n = let
	  val mfrac = 0.999 (* mass cut off at mfrac of total *)
	  val rn = real n
	  val rsc = (3.0 * Math.pi) / 16.0
	  val vsc = Math.sqrt(1.0 / rsc)
	  fun mkBodies (0, cmr, cmv, l) = let
                (* offset bodies by normalized cm coordinates.  Also, reverse
                 * the list to get the same order of bodies as in the C version.
                 *)
		val cmr = V.divvs(cmr, rn)
		val cmv = V.divvs(cmv, rn)
		fun norm ([], l) = l
		  | norm ((p as S.Body{pos, vel, ...}) :: r, l) = (
		      pos := V.subv(!pos, cmr);
		      vel := V.subv(!vel, cmv);
		      norm (r, p::l))
		in
		  norm (l, [])
		end
	    | mkBodies (i, cmr, cmv, l) = let
		val r = 1.0 / Math.sqrt (Math.pow(xrand(0.0, mfrac), ~2.0/3.0) - 1.0)
		val pos = pickshell (rsc * r)
		fun vN () = let		(* von Neumann technique *)
		      val x = xrand(0.0,1.0)
		      val y = xrand(0.0,0.1)
		      in
			if (y > x*x * (Math.pow (1.0-x*x, 3.5))) then vN () else x
		      end
		val v = ((Math.sqrt 2.0) * vN()) / Math.pow(1.0 + r*r, 0.25)
		val vel = pickshell (vsc * v)
		val body = S.Body{
			mass = 1.0 / rn,
			pos = ref pos,
			vel = ref vel,
			acc = ref V.zerov,
			phi = ref 0.0
		      }
		in
		  mkBodies (i-1, V.addv(cmr, pos), V.addv(cmv, vel), body :: l)
		end
	  in
	    mkBodies (n, V.zerov, V.zerov, [])
	  end (* testdata *)

    (* advance N-body system one time-step. *)
    fun stepSystem output {plist, dtime, eps, nstep, rmin, rsize, tnow, tol} = let
	  val dthf = 0.5 * dtime
	  val S.Space{rmin, rsize, root} = L.makeTree (plist, rmin, rsize)
	(* recalculate accelaration *)
	  fun recalc ([], n2bcalc, nbccalc, selfint) = (n2bcalc, nbccalc, selfint)
	    | recalc (p::r, n2bcalc, nbccalc, selfint) = let
		val S.Body{acc as ref acc1, vel, ...} = p
		val {n2bterm, nbcterm, skipSelf} = G.hackGrav {
			body = p, root = root, rsize = rsize, eps = eps, tol = tol
		      }
		in
		  if (nstep > 0)
		    then (* use change in accel to make 2nd order *)
			 (* correction to vel. *)
		      vel := V.addv(!vel, V.mulvs(V.subv(!acc, acc1), dthf))
		    else ();
		  recalc (r, n2bcalc+n2bterm, nbccalc+nbcterm,
		    if skipSelf then selfint else (selfint+1))
		end
	  (* advance bodies *)
	  fun advance (S.Body{pos, acc, vel, ...}) = let
		val dvel = V.mulvs (!acc, dthf)
		val vel1 = V.addv (!vel, dvel)
		val dpos = V.mulvs (vel1, dtime)
		in
		  pos := V.addv (!pos, dpos);
		  vel := V.addv (vel1, dvel)
		end
	  val (n2bcalc, nbccalc, selfint) = recalc (plist, 0, 0, 0)
	  in
	    output {nstep=nstep, tnow=tnow, n2bcalc=n2bcalc, nbccalc=nbccalc, selfint=selfint};
	    app advance plist;
	    (nstep+1, tnow + dtime)
	  end

    (* given an initial configuration, run the simulation *)
    fun go {
	  output, bodies, tnow, tstop,
	  dtime, eps, tol, rsize, rmin
	} = let
	  val step = stepSystem output
	  fun loop (nstep, tnow) = if (tnow < tstop + (0.1 * dtime))
		then loop (step {
		    plist = bodies, dtime = dtime, eps = eps, nstep = nstep,
		    rmin = rmin, rsize = rsize, tnow = tnow, tol = tol
		  })
		else ()
	  in
	    loop (0, tnow)
	  end

  end
