(* main.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure Main : sig

    include BMARK

    val dumpPS : TextIO.outstream -> unit

  end = struct

    val name = "tsp"

    val divideSz = ref 150

    fun printLength (outS, Tree.NULL) = print "(* 0 points *)\n"
      | printLength (outS, start as Tree.ND{next, x, y, ...}) = let
	  fun cycle (Tree.ND{next=next', ...}) = (next = next')
	    | cycle _ = false
	  fun distance (ax, ay, bx, by) = let
		val dx = ax-bx and dy = ay-by
		in
		  Math.sqrt (dx*dx + dy*dy)
		end
	  fun length (Tree.NULL, px, py, n, len) = (n, len+distance(px, py, x, y))
	    | length (t as Tree.ND{x, y, next, ...}, px, py, n, len) =
		if (cycle t)
		  then (n, len+distance(px, py, x, y))
		  else length(!next, x, y, n+1, len+distance(px, py, x, y))
	  in
	    if (cycle(!next))
	      then TextIO.output (outS, "(* 1 point *)\n")
	      else let
		val (n, len) = length(!next, x, y, 1, 0.0)
		in
		  TextIO.output (outS, concat[
		      "(* ", Int.toString n, "points, cycle length = ",
		      Real.toString len, " *)\n"
		    ])
		end
	  end

    fun mkTree n = BuildTree.buildTree {
	    n=n, dir=BuildTree.X_AXIS,
	    min_x=0.0, max_x=1.0,
	    min_y=0.0, max_y=1.0
	  }

    fun doit' n = TSP.tsp (mkTree n, !divideSz)

    fun dumpPS outS = (
	  TextIO.output (outS, "newgraph\n");
	  TextIO.output (outS, "newcurve pts\n");
	  Tree.printList (outS, doit' 262143);
	  TextIO.output (outS, "linetype solid\n"))

    fun testit strm = printLength (strm, doit' 32767)

    fun lp 0 = ()
      | lp n = (ignore (doit' 262143); lp(n-1))

    fun doit () = lp 25

  end

