(* main.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure Main : BMARK =
  struct

    val name = "boyer"

    open Terms;
    open Boyer;

    val subst = [
            Bind(23,
                 Prop
                  (get "f",
                   [Prop
                    (get "plus",
                     [Prop (get "plus",[Var 0, Var 1]),
                      Prop (get "plus",[Var 2, Prop (get "zero",[])])])])),
            Bind(24,
                 Prop
                  (get "f",
                   [Prop
                    (get "times",
                     [Prop (get "times",[Var 0, Var 1]),
                      Prop (get "plus",[Var 2, Var 3])])])),
            Bind(25,
                 Prop
                  (get "f",
                   [Prop
                    (get "reverse",
                     [Prop
                      (get "append",
                       [Prop (get "append",[Var 0, Var 1]),
                        Prop (get "nil",[])])])])),
            Bind(20,
                 Prop
                  (get "equal",
                   [Prop (get "plus",[Var 0, Var 1]),
                    Prop (get "difference",[Var 23, Var 24])])),
            Bind(22,
                 Prop
                  (get "lt",
                   [Prop (get "remainder",[Var 0, Var 1]),
                    Prop (get "member",[Var 0, Prop (get "length",[Var 1])])]))
          ]

    val term = Prop
                (get "implies",
                 [Prop
                  (get "and",
                   [Prop (get "implies",[Var 23, Var 24]),
                    Prop
                    (get "and",
                     [Prop (get "implies",[Var 24, Var 25]),
                      Prop
                      (get "and",
                       [Prop (get "implies",[Var 25, Var 20]),
                        Prop (get "implies",[Var 20, Var 22])])])]),
                  Prop (get "implies",[Var 23, Var 22])])

    fun testit outstrm = if tautp (apply_subst subst term)
	  then Log.print "OK\n"
	  else Log.print "FAIL\n"

    fun loop n = if n <= 0
          then ()
          else (tautp (apply_subst subst term); loop (n - 1))

    fun doit () = loop 1300

  end; (* Main *)


