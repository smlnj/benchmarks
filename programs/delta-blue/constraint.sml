(* constraint.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure Constraint : sig

    include CONSTRAINT_BASE

    (* Activate the constraint and attempt to satisfy it *)
    val add : t * Planner.t -> unit

    (* Deactivate this constraint, remove it from the constraint graph,
     * possibly causing other constraints to be satisfied, and destroy it.
     *)
    val destroy : t * Planner.t -> unit

(*
    val toString : t -> string
*)

  end = struct

    open ConstraintBase

    (* Activate the constraint and attempt to satisfy it *)
    fun add (c, planner) = (
          addToGraph c;
          Planner.incrementalAdd(planner, c))

    fun destroy (c, planner) = (
          if (isSatisfied c) then Planner.incrementalRemove(planner, c) else ();
          removeFromGraph c)

(*
    fun toString c =
          if isSatisfied c
            then concat [
                "Satisfied(", inputsToString c, " -> ",
                Variable.toString(output c), ")"
              ]
            else concat [
                "Unsatisfied(", inputsToString c, ")"
              ]
*)
val toString = fn c => if isSatisfied c
      then concat ["Satisfied(", ConstraintBase.toString c, ")"]
      else concat ["Unsatisfied(", ConstraintBase.toString c, ")"]

  end
