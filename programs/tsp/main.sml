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

    val results : string list = []

    val divideSz = ref 150

    fun printLength (Tree.NULL) = Log.print "(* 0 points *)\n"
      | printLength (start as Tree.ND{next, x, y, ...}) = let
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
	      then Log.print "(* 1 point *)\n"
	      else let
		val (n, len) = length(!next, x, y, 1, 0.0)
		in
		  Log.say [
		      "(* ", Int.toString n, "points, cycle length = ",
		      Real.toString len, " *)\n"
		    ]
		end
	  end

    fun mkTree n = BuildTree.buildTree {
	    n=n, dir=BuildTree.X_AXIS,
	    min_x=0.0, max_x=1.0,
	    min_y=0.0, max_y=1.0
	  }

    fun doit' n = TSP.tsp (mkTree n, !divideSz)

    fun dumpPS outS = (
	  Log.print "newgraph\n";
	  Log.print "newcurve pts\n";
	  Tree.printList (doit' 262143);
	  Log.print "linetype solid\n")

    fun testit () = printLength (doit' 32767)

    fun lp 0 = ()
      | lp n = (ignore (doit' 262143); lp(n-1))

    fun doit () = lp 25

  end

