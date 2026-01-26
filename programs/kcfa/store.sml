(* store.sml
 *
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

(* A store (or a heap/memory) maps addresses to denotable values. *)
structure Store : sig

    type t

    val empty : t

    val compare : t * t -> order

    val lookup : t * Addr.t -> D.t

  (* `update (st, a, d)` updates store by joining st(a) with d in the store. *)
    val update : t * Addr.t * D.t -> t

    val update' : t * Addr.t list * D.t list -> t

    val join : t * t -> t

    val foldi : (Addr.t * D.t * 'a -> 'a) -> 'a -> t -> 'a

  end = struct

    structure Map = RedBlackMapFn (
      struct
	type ord_key = Addr.t
	val compare = Addr.compare
      end)

    type t = D.t Map.map
    val empty : t = Map.empty
    val compare = Map.collate D.compare
    fun lookup (st, adr) = (case Map.find(st, adr)
	   of SOME dv => dv
	    | NONE => raise Fail("Store.lookup " ^ Bind.toString adr)
	  (* end case *))
    val update = Map.insertWith D.join
    fun update' (st, adrs, dvs) = ListPair.foldl
	  (fn (adr, dv, st) => update(st, adr, dv))
          st (adrs, dvs)
    val join = Map.unionWith D.join
    val foldi = Map.foldli

  end
