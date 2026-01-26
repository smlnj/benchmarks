(* main.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure Main : BMARK =
  struct

    val name = "kcfa"

    val results = []

    fun prSummary (summary : (Var.t * CPS.aexp list) list) = let
          val l2s = Label.toString
          val v2s = Var.toString
          fun ae2s (CPS.VAR x, acc) = v2s x :: acc
            | ae2s (CPS.LAMBDA(lab, xs, body), acc) =
                ")" :: call2s (body,
                  ") " :: params2s (xs,
                    ":(fn (" :: l2s lab :: acc))
          and call2s (CPS.CALL(lab, f, args), acc) =
                ")" :: args2s (args, " " :: ae2s(f, ":(app " :: l2s lab :: acc))
            | call2s (CPS.HALT(lab, x), acc) =
                ")" :: Var.toString x :: ":(halt " :: l2s lab :: acc
          and params2s (xs, acc) = String.concatWithMap " " v2s xs :: acc
          and args2s ([], acc) = acc
            | args2s (arg::args, acc) = args2s(args, ae2s (arg, " "::acc))
          fun prBind (x, []) = Log.say [v2s x, " -> {}\n"]
            | prBind (x, [e]) =
                Log.say (v2s x :: " -> { " :: List.rev(ae2s(e, [" }\n"])))
            | prBind (x, es) = (
                Log.say [v2s x," -> {\n"];
                List.app (fn e => Log.say("    " :: ae2s(e, ["\n"]))) es;
                Log.print "  }\n")
          in
            List.app prBind summary
          end

    fun testit () = prSummary (Analysis.analyze Examples.stdExample)

    fun doit () = ignore (Analysis.analyze Examples.bigExample)

  end (* Main *)
