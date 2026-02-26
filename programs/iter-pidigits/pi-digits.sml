(* pi-digits.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure PiDigits : sig

    val generate : int -> unit

  end = struct

    fun generate nDigits = let
          fun lp (w, n1, n2, d, col, k) = let
                val u = IntInf.quot(n1, d)
                val v = IntInf.quot(n2, d)
                in
                  if (u = v)
                    then let
                      val () = Log.print(IntInf.toString u);
                      val col = col+1
                      in
                        if (col mod 10 = 0)
                          then Log.say["\t:", Int.toString col, "\n"]
                          else ();
                        if (col = nDigits)
                          then ()
                          else let
                            val u = d * (~10 * u)
                            val n1 = 10 * n1 + u
                            val n2 = 10 * n2 + u
                            in
                              lp (w, n1, n2, d, col, k)
                            end
                      end
                    else let
                      val k2 = 2 * k
                      val w = n1 * IntInf.fromInt(k - 1)
                      val n1 = n1 * IntInf.fromInt(k2 - 1) + n2 + n2
                      val n2 = w + n2 * IntInf.fromInt(k + 2)
                      val d = d * IntInf.fromInt(k2 + 1)
                      in
                        lp (w, n1, n2, d, col, k+1)
                      end
                end
          in
            lp (0, 4, 3, 1, 0, 1);
            case (nDigits mod 10)
             of 0 => ()
              | n => Log.say[
                    StringCvt.padLeft #" " (10 - n) "\t:", Int.toString nDigits, "\n"
                  ]
          end

  end
