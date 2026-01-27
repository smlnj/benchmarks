(* rep-types.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *
 * Internal representation of variables and constraints.
 *)

structure RepTypes =
  struct

    datatype variable = Variable of {
        name : string,                          (* variable name *)
        value : int ref,                        (* current value *)
        constraints : constraint list ref,      (* constraints that depend on this variable *)
        determinedBy : constraint ref,          (* the constraint that determines
                                                 * the value of this variable.
                                                 *)
        walkStrength : Strength.t ref,          (* walkabout strength *)
        stay : bool ref,                        (* true if this is a planning-time constant *)
        mark : int ref                          (* used by the planner to mark constraints *)
      }

    and constraint = Constraint of {
        strength : Strength.t ref,
        isInput : bool,
        execute : constraint -> unit,
        isSatisfied : constraint -> bool,
        markUnsatisfied : constraint -> unit,
        addToGraph : constraint -> unit,
        removeFromGraph : constraint -> unit,
        chooseMethod : constraint * int -> unit,
        markInputs : constraint * int -> unit,
        inputsKnown : constraint * int -> bool,
        output : constraint -> variable,
        recalculate : constraint -> unit,
        inputsToString : constraint -> string,
        toString : constraint -> string
      }

    (* directions for binary constraints *)
    datatype direction = Backward | NoDirection | Forward

  end
