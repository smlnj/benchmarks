(* main.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure Main : BMARK =
  struct

    val name = "sat"

    fun AND [] = true
      | AND (false::_) = false
      | AND (true::bs) = AND bs

    fun OR [] = false
      | OR (true::_) = true
      | OR (false::bs) = OR bs

    fun phi (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) = AND [
            OR[x1, x2],
            OR[x1, not x2, not x3],
            OR[x3, x4],
            OR[not x4, x1],
            OR[not x2, not x3],
            OR[x4, x2],
            OR[not x5, x1, x2],
            OR[not x2, not x6],
            OR[not x4, x7]
          ]

    fun checkPhi (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) = let
          fun b2s true = "T" | b2s false = "F"
          val res = phi(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)
          in
            Log.say [
                "φ(", String.concatWithMap "," b2s [x1, x2, x3, x4, x5, x6, x7, x8, x9, x10],
                ") = ", b2s res, "\n"
              ];
            res
          end

    fun solve () = let
          fun try f = f true orelse f false
          in
            try (fn x1 =>
              try (fn x2 =>
                try (fn x3 =>
                  try (fn x4 =>
                    try (fn x5 =>
                      try (fn x6 =>
                        try (fn x7 =>
                          try (fn x8 =>
                            try (fn x9 =>
                              try (fn x10 =>
                                checkPhi (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)))))))))))
          end

    fun testit () = ignore(solve ())

    fun doit () = let
          fun lp (0, k) = ()
            | lp (n, k) = if solve () then lp (n-1, k+1) else lp (n-1, k)
          in
            lp (1000000, 0)
          end

  end
