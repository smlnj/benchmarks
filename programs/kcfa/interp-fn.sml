(* interp-fn.sml
 *
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

(* k-CFA parameters for k=1
 * Change these to alter the behavior of the analysis.
 *)
structure Params =
  struct

    val k = 1

    fun tick (CPS.CALL(lab, _, _), t) = [lab] (* == List.take (lab :: t, k) *)
      | tick (CPS.HALT(lab, _), t) = [lab]

    fun alloc t x = Addr.new(x, t)

  end

signature INTERP =
  sig

    val atomEval : BEnv.t * Store.t -> CPS.aexp -> D.t

    val next : StateSpace.t -> StateSpace.t list

  end

(* k-CFA abstract interpreter for k=1 *)
structure AbsInterp : INTERP =
  struct

    fun atomEval (benv : BEnv.t, store : Store.t) = let
	  fun eval (CPS.VAR x) = Store.lookup (store, BEnv.lookup(benv, x))
	    | eval (CPS.LAMBDA lam) = D.make(Closure.Clos(lam, benv))
	  in
	    eval
	  end

    fun next (StateSpace.State(call as CPS.CALL(lab, f, args), benv, store, time)) = let
	  val time' = Params.tick (call, time)
	  val procs = atomEval (benv, store) f
	  val params = List.map (atomEval (benv, store)) args
	  fun apply (Closure.Clos(lam, benv'), states) = let
		val (_, formals, call') = lam
		val bindings = List.map (Params.alloc time') formals
		val benv'' = BEnv.extend' (benv', formals, bindings)
		val store' = Store.update' (store, bindings, params)
		in
		  StateSpace.State(call', benv'', store', time') :: states
		end
	  in
	    D.fold apply [] procs
	  end
      | next _ = []

  end (* AbsInterp *)
