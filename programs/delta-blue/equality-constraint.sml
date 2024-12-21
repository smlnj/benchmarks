(* equality-constraint.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure EqualityConstraint : sig

    (* the constraint `var2 = var1` *)
    val new : Planner.t * Variable.t * Variable.t * Strength.t -> Constraint.t

  end = struct

    structure V = Variable
    structure C = Constraint
    structure S = Strength

    datatype t = datatype RepTypes.constraint

    datatype direction = datatype RepTypes.direction

    fun make (var1, var2, s) = let
          val strength = ref s
          val direction = ref NoDirection
          fun output _ = (case !direction of Forward => var2 | _ => var1)
          fun input _ = (case !direction of Forward => var1 | _ => var2)
          fun execute c = V.setValue(output c, V.getValue(input c))
          fun chooseMethod (c, mark) = let
                fun setDir (a, b, dir) =
                      if (V.getMark b <> mark)
                      andalso S.stronger(!strength, V.getWalkStrength b)
                        then direction := dir
                        else direction := NoDirection
                in
                  if (V.getMark var1 = mark)
                    then setDir (var1, var2, Forward)
                  else if (V.getMark var2 = mark)
                    then setDir (var2, var1, Backward)
                  (* If we get here, neither variable is marked, so we have a choice. *)
                  else if S.weaker(V.getWalkStrength var1, V.getWalkStrength var2)
                    then if S.stronger(!strength, V.getWalkStrength var1)
                      then direction := Backward
                      else direction := NoDirection
                    else if S.stronger(!strength, V.getWalkStrength var2)
                      then direction := Forward
                      else direction := NoDirection
                end
          in
            Constraint{
                strength = strength,
                isInput = false,
                execute = execute,
                isSatisfied = fn _ => (case !direction of NoDirection => false | _ => true),
                markUnsatisfied = fn _ => direction := NoDirection,
                addToGraph = fn c => (
                    direction := NoDirection;
                    V.addConstraint (var1, c);
                    V.addConstraint (var2, c)),
                removeFromGraph = fn c => (
                    V.removeConstraint (var1, c);
                    V.removeConstraint (var2, c);
                    direction := NoDirection),
                chooseMethod = chooseMethod,
                markInputs = fn (c, mark) => V.setMark(input c, mark),
                inputsKnown = fn (c, mark) => let
                    val i = input c
                    in
                      (V.getMark i = mark) orelse V.getStay i
                        orelse C.same(V.getDeterminedBy i, C.null)
                    end,
                output = output,
                recalculate = fn c => let
                    val inp = input c
                    val outp = output c
                    in
                      V.setWalkStrength (outp, S.weakestOf(!strength, V.getWalkStrength inp));
                      V.setStay (outp, V.getStay inp);
                      if V.getStay outp then execute c else ()
                    end,
                inputsToString = fn c => V.toString(input c)
              }
          end

    fun new (planner, var1, var2, s) = let
          val c = make (var1, var2, s)
          in
            C.addToGraph c;
            Planner.incrementalAdd (planner, c);
            c
          end

  end
