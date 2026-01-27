(* delta-blue.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure DeltaBlue : sig

    (* This is the standard DeltaBlue benchmark. A long chain of
     * equality constraints is constructed with a stay constraint on
     * one end. An edit constraint is then added to the opposite end
     * and the time is measured for adding and removing this
     * constraint, and extracting and executing a constraint
     * satisfaction plan. There are two cases. In case 1, the added
     * constraint is stronger than the stay constraint and values must
     * propagate down the entire length of the chain. In case 2, the
     * added constraint is weaker than the stay constraint so it cannot
     * be accomodated. The cost in this case is, of course, very
     * low. Typical situations lie somewhere between these two
     * extremes.
    *)
    val chainTest : int -> unit

    (* This test constructs a two sets of variables related to each
     * other by a simple linear transformation (scale and offset). The
     * time is measured to change a variable on either side of the
     * mapping and to change the scale and offset factors.
     *)
    val projectionTest : int -> unit

  end = struct

    structure V = Variable
    structure C = Constraint
    structure S = Strength

    fun newVar (name, value) = V.new(name, value, S.weakest)

    fun chainTest n = let
val _ = print "# Chain Test\n"
          val planner = Planner.new()
          (* build a chain of `n` equality constraints *)
          val first = newVar("v0", 0)
          fun build (i, prev) = if (i < n)
                then let
                  val x = newVar("v"^Int.toString i, 0)
                  in
                    EqualityConstraint.new(planner, prev, x, S.required);
                    build (i+1, x)
                  end
                else prev
val _ = print "## build\n"
          val last = build(1, first)
handle ex => raise ex
          val _ = StayConstraint.new(planner, last, S.strongDefault)
handle ex => raise ex
          val editC = EditConstraint.new(planner, first, S.preferred)
handle ex => raise ex
          val plan = Planner.extractPlanFromConstraints(planner, [editC])
handle ex => raise ex
          (* execute the plan 100 times *)
          fun execLp i = if (i < 100)
                then (
                  Variable.setValue(first, i);
                  Planner.execute plan;
                  if (V.getValue last <> i)
                    then raise Fail "Chain test failed!"
                    else execLp(i+1))
                else ()
          in
print "## execute\n";
            execLp 0;
            C.destroy (editC, planner)
          end

    fun change (planner, x, newValue) = let
          val editC = EditConstraint.new (planner, x, S.preferred)
          val plan = Planner.extractPlanFromConstraints (planner, [editC])
          fun lp i = if (i < 10)
                then (
                  Variable.setValue (x, newValue);
                  Planner.execute plan;
                  lp (i+1))
                else ()
          in
            lp 0;
            C.destroy (editC, planner)
          end

    fun existsi pred l = let
          fun chk (_, []) = false
            | chk (i, x::xs) = pred(i, x) orelse chk(i+1, xs)
          in
            chk (0, l)
          end

    fun projectionTest n = let
          val planner = Planner.new()
          val scale = newVar("scale", 10)
          val offset = newVar("offset", 1000)
          (* set up the constraints *)
          val src = newVar ("src0", 0)
          val dst = newVar ("dst0", 0)
          fun lp (i, src, dst, dests) = if (i < n)
                then let
                  val src = newVar("src" ^ Int.toString i, i)
                  val dst = newVar("dst" ^ Int.toString i, i)
                  in
                    StayConstraint.new(planner, src, S.normal);
                    ScaleConstraint.new(planner, {
                        src=src, scale=scale, offset=offset, dest=dst,
                        strength=S.required
                      });
                    lp (i+1, src, dst, dst::dests)
                  end
                else (src, dst, List.rev dests)
          val (src, dst, dests) = lp (1, src, dst, [dst])
          in
            (* test 1 *)
print "# Projection Test 1\n";
            change (planner, src, 17);
            if (Variable.getValue dst <> 1170)
              then raise Fail "Projection test 1 failed!"
              else ();
            (* test 2 *)
print "# Projection Test 2\n";
            change (planner, dst, 1050);
            if (Variable.getValue dst <> 5)
              then raise Fail "Projection test 2 failed!"
              else ();
            (* test 3 *)
print "# Projection Test 3\n";
            change(planner, scale, 5);
            let fun lp (i, x::xs) = if (i < n - 1)
                      then if (Variable.getValue x <> i * 5 + 1000)
                        then raise Fail "Projection test 3 failed!"
                        else lp (i+1, xs)
                      else ()
                  | lp _ = ()
                in
                  lp (0, dests)
                end;
            (* test 4 *)
print "# Projection Test 4\n";
            change(planner, offset, 2000);
            let fun lp (i, x::xs) = if (i < n - 1)
                      then if (Variable.getValue x <> i * 5 + 2000)
                        then raise Fail "Projection test 4 failed!"
                        else lp (i+1, xs)
                      else ()
                  | lp _ = ()
                in
                  lp (0, dests)
                end
          end

  end
