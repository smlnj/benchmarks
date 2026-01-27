(* edit-constraint.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure EditConstraint : sig

    val new : Planner.t * Variable.t * Strength.t -> Constraint.t

  end = struct

    structure V = Variable
    structure C = Constraint

    datatype t = datatype RepTypes.constraint

    fun make (out, s) = let
          val satisfied = ref false
          val strength = ref s
          in
            Constraint{
                strength = strength,
                isInput = true,
                execute = fn _ => (),
                isSatisfied = fn _ => !satisfied,
                markUnsatisfied = fn _ => satisfied := false,
                addToGraph = fn c => (
                    V.addConstraint (out, c);
                    satisfied := false),
                removeFromGraph = fn c => (
                    V.removeConstraint (out, c);
                    satisfied := false),
                chooseMethod = fn (c, mark) =>
                    satisfied := ((V.getMark out <> mark)
                      andalso Strength.stronger(!strength, V.getWalkStrength out)),
                markInputs = fn _ => (),
                inputsKnown = fn _ => true,
                output = fn _ => out,
                recalculate = fn c => (
                    (* optimized by removing call to execute *)
                    V.setWalkStrength (out, !strength);
                    V.setStay (out, false)),
                inputsToString = fn _ => "",
                toString = fn _ => V.toString out ^ " == EDIT"
              }
          end

    fun new (planner, out, s) = let
          val c = make (out, s)
          in
            C.addToGraph c;
            Planner.incrementalAdd (planner, c);
            c
          end

  end
