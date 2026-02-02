(* scale-constraint.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure ScaleConstraint : sig

    (* the constraint `dest = scale * src + offset` *)
    val new : Planner.t * {
            src : Variable.t,
            scale : Variable.t,
            offset : Variable.t,
            dest : Variable.t,
            strength : Strength.t
          } -> Constraint.t

  end = struct

    structure V = Variable
    structure C = Constraint
    structure S = Strength

    datatype t = datatype RepTypes.constraint

    datatype direction = datatype RepTypes.direction

(* var1 = src, var2 = dest *)
    fun make {src, scale, offset, dest, strength} = let
          val strength = ref strength
          val direction = ref NoDirection
          fun output _ = (case !direction of Forward => dest | _ => src)
          fun input _ = (case !direction of Forward => src | _ => dest)
          fun execute c = (case !direction
                 of Forward => V.setValue(
                      dest,
                      V.getValue src * V.getValue scale + V.getValue offset)
                  | _ => V.setValue(
                      src,
                      Int.quot(V.getValue dest - V.getValue offset, V.getValue scale))
                (* end case *))
          fun chooseMethod (c, mark) = let
                fun setDir (a, b, dir) =
                      if (V.getMark b <> mark)
                      andalso S.stronger(!strength, V.getWalkStrength b)
                        then direction := dir
                        else direction := NoDirection
                in
                  if (V.getMark var1 = mark)
                    then setDir (var1, var2, Forward)
                    else ();
                  if (V.getMark var2 = mark)
                    then setDir (var2, var1, Backward)
                    else ();
                  (* If we get here, neither variable is marked, so we have a choice. *)
                  if S.weaker(V.getWalkStrength var1, V.getWalkStrength var2)
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
                    V.addConstraint (src, c);
                    V.addConstraint (scale, c);
                    V.addConstraint (offset, c);
                    V.addConstraint (dest, c)),
                removeFromGraph = fn c => (
                    V.removeConstraint (src, c);
                    V.removeConstraint (scale, c);
                    V.removeConstraint (offset, c);
                    V.removeConstraint (dest, c);
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
                inputsToString = fn c => V.toString(input c),
                toString = fn _ => (case !direction
                   of Forward => concat [
                          V.toString dest, " == ", V.toString scale, " * ",
                          V.toString src, " + ", V.toString offset
                        ]
                    | _ => concat [
                          V.toString src, " == (", V.toString dest, " - ",
                          V.toString offset, ") / ", V.toString scale
                        ]
                  (* end case *))
              }
          end

    fun new (planner, arg) = let
          val c = make arg
          in
            C.addToGraph c;
            Planner.incrementalAdd (planner, c);
            c
          end

  end
