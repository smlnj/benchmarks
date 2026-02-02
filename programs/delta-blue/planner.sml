(* planner.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https: *www.smlnj.org)
 * All rights reserved.
 *)

structure Planner : sig

    type t

    type plan = ConstraintBase.t list

    val new : unit -> t

    (* Attempt to satisfy the given constraint and, if successful,
     * incrementally update the dataflow graph.  Details: If satifying
     * the constraint is successful, it may override a weaker constraint
     * on its output. The algorithm attempts to resatisfy that
     * constraint using some other method. This process is repeated
     * until either a) it reaches a variable that was not previously
     * determined by any constraint or b) it reaches a constraint that
     * is too weak to be satisfied using any of its methods. The
     * variables of constraints that have been processed are marked with
     * a unique mark value so that we know where we've been. This allows
     * the algorithm to avoid getting into an infinite loop even if the
     * constraint graph has an inadvertent cycle.
     *)
    val incrementalAdd : t * ConstraintBase.t -> unit

    (* Entry point for retracting a constraint. Remove the given
     * constraint and incrementally update the dataflow graph.
     * Details: Retracting the given constraint may allow some currently
     * unsatisfiable downstream constraint to be satisfied. We therefore collect
     * a list of unsatisfied downstream constraints and attempt to
     * satisfy each one in turn. This list is traversed by constraint
     * strength, strongest first, as a heuristic for avoiding
     * unnecessarily adding and then overriding weak constraints.
     * Assume: c is satisfied.
     *)
    val incrementalRemove : t * ConstraintBase.t -> unit

    (* Extract a plan for resatisfaction starting from the outputs of
     * the given constraints, usually a set of input constraints.
     *)
    val extractPlanFromConstraints : t * ConstraintBase.t list -> plan

    val execute : plan -> unit

  end = struct

    structure V = Variable
    structure C = ConstraintBase
    structure S = Strength

    datatype t = Planner of {
        currentMark : int ref
      }

    type plan = C.t list

    fun new () = Planner{currentMark = ref 0}

    fun newMark (Planner{currentMark}) = let
          val n = !currentMark
          in currentMark := n+1; n end

    fun addConstraintsConsumingTo (x, constraints) = let
          val determiningC = V.getDeterminedBy x
          fun lp ([], cs) = constraints @ cs
            | lp (c::cc, cs) =
                if not(C.same(c, determiningC)) andalso C.isSatisfied c
                  then lp (cc, c::cs)
                  else lp (cc, cs)
          in
            lp (V.getConstraints x, [])
          end

    (* Attempt to find a way to enforce this constraint. If successful,
     * record the solution, perhaps modifying the current dataflow
     * graph. Answer the constraint that this constraint overrides, if
     * there is one, or nil, if there isn't.
     * Assume: the constraint is not already satisfied.
     *)
    (* Note: in the Java implementation, this function is part of the Constraint
     * class, but that would create a cyclic depedency between it and the Planner
     * module.
     *)
    fun satisfy (planner, c, mark) = (
          C.chooseMethod (c, mark);
print(concat["## satisfy: ", C.toString c, "; mark = ", Int.toString mark, "\n"]);
          if (not (C.isSatisfied c))
            then if Strength.same(C.getStrength c, Strength.required)
              then raise Fail "Could not satisfy a required constraint"
              else C.null
            else let (* constraint can be satisfied *)
              (* mark inputs to allow cycle detection in `addPropagate` *)
              val _ = C.markInputs (c, mark)
              val out = C.output c
              val overridden = V.getDeterminedBy out
              in
(*DEBUG*)print(concat["### overridden = ", C.toString overridden, "\n"]);
                if not(C.same(overridden, C.null))
                  then C.markUnsatisfied c
                  else ();
                V.setDeterminedBy (out, c);
                if addPropagate (planner, c, mark)
                  then raise Fail "Cycle encountered"
                  else ();
                V.setMark (out, mark);
                overridden
(*DEBUG*)before print "## satisfy done\n"
              end)

    and incrementalAdd (planner, c) = let
          val mark = newMark planner
          fun propagate overridden = (
(*DEBUG*)print(concat["### propagate ", C.toString overridden, "\n"]);
if C.same(overridden, C.null)
                then ()
                else propagate (satisfy (planner, overridden, mark))
(*DEBUG*))
          in
print(concat["## incrementalAdd: ", C.toString c, "; mark = ", Int.toString mark, "\n"]);
            propagate (satisfy (planner, c, mark))
          end
handle ex => raise ex

    and removePropagateFrom (planner, out) = let
          val () = V.setDeterminedBy (out, C.null)
          val () = V.setWalkStrength (out, S.weakest)
          val () = V.setStay (out, true)
          fun loop ([], unsatisfied) = unsatisfied
            | loop (x :: todo, unsatisfied) = let
                val unsatisfied = List.foldl
                      (fn (c, u) => if C.isSatisfied c then u else c::u)
                        unsatisfied
                          (V.getConstraints x)
                val determiningC = V.getDeterminedBy x
                val todo = List.foldl
                      (fn (nextC, td) =>
                        if not(C.same(nextC, determiningC)) andalso C.isSatisfied nextC
                          then (C.recalculate nextC; C.output nextC :: td)
                          else td)
                        todo
                          (V.getConstraints x)
                in
                  loop (todo, unsatisfied)
                end
          in
            loop ([out], [])
          end

    and incrementalRemove (planner, c) = let
          val out = C.output c
          val () = C.markUnsatisfied c
          val () = C.removeFromGraph c
          val unsatisfied = removePropagateFrom (planner, out)
          fun lp strength = if S.same(strength, S.weakest)
                then ()
                else let
                  fun add c' = if S.same(strength, C.getStrength c')
                        then incrementalAdd (planner, c')
                        else ()
                  in
                    List.app add unsatisfied;
                    lp (S.nextWeaker strength)
                  end
          in
print(concat["## incrementalRemove: ", C.toString c, "\n"]);
            lp Strength.required
          end
handle ex => raise ex

    (* Recompute the walkabout strengths and stay flags of all variables
     * downstream of the given constraint and recompute the actual
     * values of all variables whose stay flag is true. If a cycle is
     * detected, remove the given constraint and answer
     * false. Otherwise, answer true.
     * Details: Cycles are detected when a marked variable is
     * encountered downstream of the given constraint. The sender is
     * assumed to have marked the inputs of the given constraint with
     * the given mark. Thus, encountering a marked node downstream of
     * the output constraint means that there is a path from the
     * constraint's output to one of its inputs.
     *)
    and addPropagate (planner, c, mark) = let
          fun lp [] = true
            | lp (d :: todo) = if V.getMark(C.output d) = mark
                then (
                  incrementalRemove (planner, c);
                  false)
                else (
                  C.recalculate d;
(*DEBUG*)print(concat["#### after recalculate: ", C.toString d, "\n"]);
                  lp (addConstraintsConsumingTo (C.output d, todo)))
          in
(*DEBUG*)print(concat["### addPropagate: c = ", C.toString c, "; mark = ", Int.toString mark, "\n"]);
            lp [c]
          end

    (* Extract a plan for resatisfaction starting from the given source
     * constraints, usually a set of input constraints. This method
     * assumes that stay optimization is desired; the plan will contain
     * only constraints whose output variables are not stay. Constraints
     * that do no computation, such as stay and edit constraints, are
     * not included in the plan.
     * Details: The outputs of a constraint are marked when it is added
     * to the plan under construction. A constraint may be appended to
     * the plan when all its input variables are known. A variable is
     * known if either a) the variable is marked (indicating that has
     * been computed by a constraint appearing earlier in the plan), b)
     * the variable is 'stay' (i.e. it is a constant at plan execution
     * time), or c) the variable is not determined by any
     * constraint. The last provision is for past states of history
     * variables, which are not stay but which are also not computed by
     * any constraint.
     * Assume: sources are all satisfied.
     *)
    fun makePlan (planner, sources) = let
          val mark = newMark planner
          fun loop ([], plan) = List.rev plan
            | loop (c::todo, plan) =
                if V.getMark(C.output c) <> mark andalso C.inputsKnown(c, mark)
                  then (
                    V.setMark(C.output c, mark);
                    loop (addConstraintsConsumingTo (C.output c, todo), c::plan))
                  else loop (todo, plan)
          in
            loop (sources, [])
          end

    fun extractPlanFromConstraints (planner, constraints) =
          makePlan (
            planner,
            List.filter (fn c => C.isInput c andalso C.isSatisfied c) constraints)

    val execute = List.app C.execute

  end
