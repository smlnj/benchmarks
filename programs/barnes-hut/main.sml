(* main.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure Main : BMARK =
  struct

    structure BH = BarnesHut3D

    val name = "barnes-hut-3d"

    val results : string list = []

    (* parameters *)
    val nbody = 1024
    val tnow = 0.0
    val tstop = 2.0
    val dtime = 0.0125
    val eps = 0.05
    val tol = 1.0
    val dtout = 0.25
    val rmin = BH.S.V.tabulate (fn _ => ~4.0)
    val rsize = 8.0
    val headline = "Plummer model"

    fun doOne () = (
	  BH.srand 123.0;
          BH.go {
	      output = fn _ => (),
	      bodies = BH.testdata nbody,
	      tnow = tnow, tstop = tstop,
	      dtime = dtime, eps = eps, tol = tol,
	      rmin = rmin,
	      rsize = rsize
	    })

    fun loop n = if n <= 0 then () else (doOne (); loop (n - 1))

    fun doit () = loop 10

    fun testit outS = let
          val nbody = 256 (* smaller problem for testing *)
          val bodies = BarnesHut3D.testdata nbody
	  fun output {nstep, tnow, n2bcalc, nbccalc, selfint} = BH.DataIO.output{
		  bodies = bodies, nbody = nbody,
		  n2bcalc = n2bcalc, nbccalc = nbccalc,
                  selfint = selfint, tnow = tnow
		}
          in
	    BH.DataIO.initOutputStrm {
		outS = outS, headline = headline, nbody = nbody, tnow = tnow,
		dtime = dtime, eps = eps, tol = tol, dtout = dtout, tstop = tstop
	      };
	    BH.go {
		output=output, bodies=bodies, tnow=tnow, tstop=tstop,
		dtime=dtime, eps=eps, tol=tol, rsize=rsize, rmin=rmin
	      }
          end
  end;
