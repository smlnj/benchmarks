(* analysis.sml
 *
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * State-space exploration.
 *)

structure Analysis : sig

    val analyze : CPS.call -> (Var.t * CPS.aexp list) list

  end = struct

    local
      fun member (sts, st) = List.exists (fn st' => StateSpace.same(st, st')) sts

    (* insert into sorted list of expressions *)
      fun insertExp (exp, []) = [exp]
	| insertExp (exp, e::exps) = (case CPS.compareExp (exp, e)
	     of LESS => e :: insertExp (exp, exps)
	      | GREATER => exp :: e :: exps
	      | EQUAL => e :: exps
	    (* end case *))
    (* monomorphic store utilities *)
      type mono_store = (Var.t * CPS.aexp list) list
      fun monoBinding (Bind.Bnd(x, _)) = x
      fun monoValue (Closure.Clos(lam, _)) = CPS.LAMBDA lam
      fun monoValues d = D.fold (fn (v, exps) => monoValue v :: exps) [] d
      fun monoStoreUpdate ([], x, exp) : mono_store = [(x, [exp])]
	| monoStoreUpdate ((y, exps)::r, x, exp) = (case Var.compare(x, y)
	     of LESS => (y, exps) :: monoStoreUpdate (r, x, exp)
	      | GREATER => (x, [exp]) :: (y, exps) :: r
	      | EQUAL => (y, insertExp(exp, exps)) :: r
	    (* end case *))
      fun monoStoreUpdate' (mst, x, exps) : mono_store =
	    List.foldl (fn (exp, mst) => monoStoreUpdate (mst, x, exp)) mst exps
    in

    fun explore (seen, []) = seen
      | explore (seen, todo as st::sts) =
	  if member (seen, st)
	    then explore (seen, sts)
	    else explore (st :: seen, AbsInterp.next st @ sts)

    fun summarize states = List.foldl
	  (fn (state, sum) => Store.join(StateSpace.storeOf state, sum))
	    Store.empty states

  (* : Store.t -> (Var.t * CPS.exp list) list *)
    fun monovariantStore st = Store.foldi
	  (fn (Bind.Bnd(x, _), d, mst) => monoStoreUpdate'(mst, x, monoValues d))
	    [] st

    fun analyze exp = let
	  val states = explore ([], [StateSpace.initState exp])
	  val summary = summarize states
	  in
	    monovariantStore summary
	  end

    end (* local *)

  end (* Analysis *)
