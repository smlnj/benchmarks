(* value.sml
 *
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

(* Closures pair a lambda term with a binding environment that
 * determines the value of its free variables.
 *)
structure Closure =
  struct
    datatype t = Clos of CPS.lambda * BEnv.t

    fun lambdaOf (Clos(lam, _)) = CPS.LAMBDA lam
    fun benvOf (Clos(_, benv)) = benv

    fun compare (Clos(lam1, benv1), Clos(lam2, benv2)) = (
	  case CPS.compareLambda (lam1, lam2)
	   of EQUAL => BEnv.compare (benv1, benv2)
	    | order => order
	  (* end case *))

  end (* Closure *)

(* For pure CPS, closures are the only kind of value. *)
structure Value =
  struct

    type t = Closure.t

    val compare = Closure.compare

  end (* Value *)

(* An abstract denotable value is a set of possible values. *)
structure D : sig

    type t

    val make : Value.t -> t

    val join : t * t -> t

    val compare : t * t -> order

    val fold : (Value.t * 'a -> 'a) -> 'a -> t -> 'a

  end = struct

    structure Set = RedBlackSetFn (
      struct
	type ord_key = Value.t
	val compare = Value.compare
      end)

    type t = Set.set
    val make = Set.singleton
    val join = Set.union
    val compare = Set.compare
    val fold = Set.foldl

  end (* D *)
