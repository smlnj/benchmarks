(* cps.sml
 *
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure CPS =
  struct

    type label = Label.t

    type var = Var.t

    datatype aexp
      = VAR of var
      | LAMBDA of lambda

    and call
      = CALL of label * aexp * aexp list
      | HALT of label * var

    withtype lambda = label * var list * call

    fun compareLambda ((lab1, _, _):lambda, (lab2, _, _):lambda) =
	  Label.compare(lab1, lab2)

    fun compareExp (VAR x, VAR y) = Var.compare(x, y)
      | compareExp (VAR _, _) = LESS
      | compareExp (_, VAR _) = GREATER
      | compareExp (LAMBDA lam1, LAMBDA lam2) = compareLambda(lam1, lam2)

    fun sameCall (CALL(l1, _, _), CALL(l2, _, _)) = (l1 = l2)
      | sameCall (HALT(l1, _), HALT(l2, _)) = (l1 = l2)
      | sameCall _ = false

  end
