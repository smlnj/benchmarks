(* tree.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *
 * Trees for the TSP program.
 *)

structure Tree =
  struct

    datatype tree
      = NULL
      | ND of {
	    left : tree, right : tree,
	    x : real, y : real,
	    sz : int,
	    prev : tree ref, next : tree ref
	  }

    fun mkNode (l, r, x, y, sz) = ND{
	    left = l, right = r, x = x, y = y, sz = sz,
	    prev = ref NULL, next = ref NULL
	  }

    fun printTree NULL = ()
      | printTree (ND{x, y, left, right, ...}) = (
	  Log.say [Real.toString x, " ", Real.toString y, "\n"];
	  printTree left;
	  printTree right)

    fun printList (NULL) = ()
      | printList (start as ND{next, ...}) = let
	  fun cycle (ND{next=next', ...}) = (next = next')
	    | cycle _ = false
	  fun prt NULL = ()
	    | prt (t as ND{x, y, next, ...}) = (
		Log.say [Real.toString x, " ", Real.toString y, "\n"];
		if (cycle (!next))
		  then ()
		  else prt (!next))
	  in
	    prt start
	  end

  end;

