(* mandelbrot-rat.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *
 * Mandelbrot sets using rational numbers instead of floating-point.
 *)

structure Main : BMARK =
  struct

    val name = "mandelbrot-rat"

    val results : string list = []

    (* rational number *)
    type fixed = int * int

    fun gcd (m, n) = if n = 0 then m else gcd (n, m mod n)

    fun fromInt n = (n, 1)

    fun shr (n, i) = Word.toIntX (Word.~>> (Word.fromInt n, i))

    fun truncate (n, d) = if Int.abs d > 0x7FFFFFFF orelse Int.abs n > 0x7FFFFFFF
          then truncate (shr (n, 0w4), shr (d, 0w4))
          else (n, d)

    fun normalize (n, d) = if n = 0
          then (0, 1)
          else let
            val g = gcd (n, d)
            in
              if g = 1
                then truncate (n, d)
                else truncate (n div g, d div g)
            end

    fun add ((n1, d1), (n2, d2)) = if d1 = d2
          then truncate (n1 + n2, d1)
          else let
            val n1' = d2 * n1
            val n2' = d1 * n2
            in
              normalize (n1' + n2', d1 * d2)
            end

    fun sub ((n1, d1), (n2, d2)) = if d1 = d2
          then truncate (n1 - n2, d1)
          else let
            val n1' = d2 * n1
            val n2' = d1 * n2
            in
              normalize (n1' - n2', d1 * d2)
            end

    fun mul ((n1, d1), (n2, d2)) = normalize (n1 * n2, d1 * d2)

    fun divide ((n, d), m) = normalize (n, d * m)

    fun isPositive (n, d) = (n > 0 andalso d > 0) orelse (n < 0 andalso d < 0)

    infix 6 ++ --
    infix 7 ** //
    infix 9 over
    val op ++ = (add : fixed * fixed -> fixed)
    val op -- = (sub : fixed * fixed -> fixed)
    val op ** = (mul : fixed * fixed -> fixed)
    val op // = (divide : fixed * int -> fixed)
    val op over = (Fn.id : int * int -> fixed)

    val x_base = ~2 over 1
    val y_base = 9 over 8
    val side = 5 over 4

    val sz = 256
    val maxCount = 512

    val delta = side // sz

    val sum_iterations = ref 0

    fun loop1 i = if (i >= sz)
          then ()
          else let
            val c_im : fixed = y_base -- (delta ** fromInt i)
            fun loop2 j = if (j >= sz)
                  then ()
                  else let
                  (* NOTE: older versions of the benchmark had the following
                   * incorrect code:
                    val c_re = x_base * (delta + real_j)
                   *)
                    val c_re = x_base ++ (delta ** fromInt j)
                    fun loop3 (count, z_re : fixed, z_im : fixed) = if (count < maxCount)
                          then let
                            val z_re_sq = z_re ** z_re
                            val z_im_sq = z_im ** z_im
                            in
                              if isPositive ((z_re_sq ++ z_im_sq) -- fromInt 4)
                                then count
                                else let
                                  val z_re_im = (z_re ** z_im)
                                  in
                                    loop3 (count+1,
                                      (z_re_sq -- z_im_sq) ++ c_re,
                                       z_re_im ++ z_re_im ++ c_im)
                                  end
                            end (* loop3 *)
                          else count
                    val count = loop3 (0, c_re, c_im)
                    in
                      sum_iterations := !sum_iterations + count;
                      loop2 (j+1)
                    end
            in
              loop2 0;
              loop1 (i+1)
            end

    fun doit () = (sum_iterations := 0; loop1 0; ())

    fun testit _ = (
          sum_iterations := 0;
          loop1 0;
          print (Int.toString(!sum_iterations) ^ " iterations\n"))

  end (* Mandelbrot *)
