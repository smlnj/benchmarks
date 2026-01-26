(* bind.sml
 *
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure Bind =
  struct

  (* A binding is minted each time a variable gets bound to a value. *)
    datatype t = Bnd of Var.t * Time.t

    fun compare (Bnd(x, t1), Bnd(y, t2)) = (case Var.compare(x, y)
	   of EQUAL => Time.compare(t1, t2)
	    | order => order
	  (* end case *))

    fun toString (Bnd(x, [])) = Var.toString x
      | toString (Bnd(x, ts)) = concat [
            Var.toString x, "@", String.concatWithMap "." Label.toString ts
          ]

  end (* Bind *)
