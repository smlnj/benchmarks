(* addr.sml
 *
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Addresses can point to values in the store.
 * In pure CPS, the only kind of addresses are bindings.
 *)

structure Addr =
  struct

    type t = Bind.t

    val new = Bind.Bnd

    val compare = Bind.compare

  end (* Addr *)

structure BEnv : sig

    type t

    val empty : t
    val compare : t * t -> order
    val lookup : t * Var.t -> Addr.t
    val extend : t  * Var.t * Addr.t -> t
    val extend' : t * Var.t list * Addr.t list -> t

  end = struct

    structure Map = Var.Map

    type t = Addr.t Map.map

    val empty : t = Map.empty

    val compare = Map.collate Addr.compare

    fun lookup (benv, x) = (case Map.find(benv, x)
	   of SOME adr => adr
	    | NONE => raise Fail "lookup"
	  (* end case *))

    val extend = Map.insert

    fun extend' (benv, xs, adrs) = ListPair.foldl
	  (fn (x, adr, benv) => Map.insert(benv, x, adr))
	    benv (xs, adrs)

  end (* BEnv *)
