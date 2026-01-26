(* state-space.sml
 *
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Abstract state space
 *)

structure StateSpace =
  struct

    datatype t = State of CPS.call * BEnv.t * Store.t * Time.t

    fun storeOf (State(_, _, st, _)) = st

    fun same (State(call1, bev1, st1, t1), State(call2, bev2, st2, t2)) =
	  Time.same(t1, t2) andalso CPS.sameCall(call1, call2)
	  andalso (BEnv.compare(bev1, bev2) = EQUAL)
	  andalso (Store.compare(st1, st2) = EQUAL)

    fun initState call = State(call, BEnv.empty, Store.empty, Time.zero)

  end (* StateSpace *)
