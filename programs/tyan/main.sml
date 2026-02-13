(* main.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure Main : BMARK =
  struct

    val name = "tyan"

    val results = []

    val _ = G.maxDeg:=1000000

    fun grab mi = MI.fold (fn ((m,g),l) => P.cons((F.one,m),!g)::l) mi []

    fun gb fs = let
          val g = G.grobner fs handle (Util.Illegal s) => (Log.print s; raise Div)
          val fs = grab g
          fun info f = Log.say [
                  M.display(P.leadMono f), " + ",
                  Int.toString(P.numTerms f - 1), " terms\n"
                ]
          in
            List.app info fs
          end

    fun doit () = let
          val u6 = List.map G.parsePoly [
                  "abcdef-g6","a+b+c+d+e+f","ab+bc+cd+de+ef+fa",
                  "abc+bcd+cde+def+efa+fab",
                  "abcd+bcde+cdef+defa+efab+fabc",
                  "abcde+bcdef+cdefa+defab+efabc+fabcd"
                ]
          fun loop 0 = ()
            | loop n = (gb u6; loop (n - 1))
           in
             loop 192
           end

    fun testit () = ()

  end
