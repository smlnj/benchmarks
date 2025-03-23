(* main.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *
 * The "Fast Fourier Transform" (FFT) benchmark (double precision).
 *)

structure Main : BMARK =
  struct

    val name = "fft64"

    structure A = Real64Array
    structure R = Real64
    structure M = R.Math

    fun say (NONE, _) = ()
      | say (SOME outstrm, s) = TextIO.output(outstrm, s)

    fun say' toS (NONE, _) = ()
      | say' toS (SOME outstrm, v) = TextIO.output(outstrm, toS v)

    val printr = say' R.toString
    val printi = say' Int.toString

    val tpi = 2.0 * M.pi

    fun fft out px py np = let
          fun find_num_points i m = if i < np then find_num_points (i+i) (m+1) else (i,m)
          val (n,m) = find_num_points 2 1
          in
            if n <> np
              then let
                fun loop i = if i > n
                      then ()
                      else (
                        A.update(px, i, 0.0);
                        A.update(py, i, 0.0);
                        loop (i+1))
                in
                  loop (np+1);
                  say (out, "Use "); printi(out, n); say(out, " point fft\n")
                end
              else ();
            let
            fun loop_k k n2 = if k >= m
                  then ()
                  else let
                    val n4 = n2 div 4
                    val e  = tpi / (R.fromInt n2)
                    fun loop_j j a = if j > n4 then () else
                      let val a3 = 3.0 * a
                          val cc1 = M.cos(a)
                          val ss1 = M.sin(a)
                          val cc3 = M.cos(a3)
                          val ss3 = M.sin(a3)
                          fun loop_is is id = if is >= n then () else
                            let fun loop_i0 i0 = if i0 >= n then () else
                              let val i1 = i0 + n4
                                  val i2 = i1 + n4
                                  val i3 = i2 + n4
                                  val r1 = A.sub(px, i0) - A.sub(px, i2)
                                  val _ = A.update(px, i0, A.sub(px, i0) + A.sub(px, i2))
                                  val r2 = A.sub(px, i1) - A.sub(px, i3)
                                  val _ = A.update(px, i1, A.sub(px, i1) + A.sub(px, i3))
                                  val s1 = A.sub(py, i0) - A.sub(py, i2)
                                  val _ = A.update(py, i0, A.sub(py, i0) + A.sub(py, i2))
                                  val s2 = A.sub(py, i1) - A.sub(py, i3)
                                  val _ = A.update(py, i1, A.sub(py, i1) + A.sub(py, i3))
                                  val s3 = r1 - s2
                                  val r1 = r1 + s2
                                  val s2 = r2 - s1
                                  val r2 = r2 + s1
                                  val _ = A.update(px, i2, r1*cc1 - s2*ss1)
                                  val _ = A.update(py, i2, ~s2*cc1 - r1*ss1)
                                  val _ = A.update(px, i3, s3*cc3 + r2*ss3)
                                  val _ = A.update(py, i3, r2*cc3 - s3*ss3)
                                  in
                                    loop_i0 (i0 + id)
                                  end
                              in
                                loop_i0 is;
                                loop_is (2 * id - n2 + j) (4 * id)
                              end
                            in
                              loop_is j (2 * n2);
                              loop_j (j+1) (e * R.fromInt j)
                            end
                    in
                      loop_j 1 0.0;
                      loop_k (k+1) (n2 div 2)
                    end
            in
              loop_k 1 n
            end;

            (************************************)
            (*  Last stage, length=2 butterfly  *)
            (************************************)
            let fun loop_is is id = if is >= n then () else
              let fun loop_i0 i0 = if i0 > n then () else
                let val i1 = i0 + 1
                    val r1 = A.sub(px, i0)
                    val _ = A.update(px, i0, r1 + A.sub(px, i1))
                    val _ = A.update(px, i1, r1 - A.sub(px, i1))
                    val r1 = A.sub(py, i0)
                    val _ = A.update(py, i0, r1 + A.sub(py, i1))
                    val _ = A.update(py, i1, r1 - A.sub(py, i1))
                in
                  loop_i0 (i0 + id)
                end
              in
                loop_i0 is;
                loop_is (2*id - 1) (4 * id)
              end
            in
              loop_is 1 4
            end;

            (*************************)
            (*  Bit reverse counter  *)
            (*************************)
            let fun loop_i i j = if i >= n then () else
             (if i < j then
               (let val xt = A.sub(px, j)
                in A.update(px, j, A.sub(px, i)); A.update(px, i, xt)
                end;
                let val xt = A.sub(py, j)
                in A.update(py, j, A.sub(py, i)); A.update(py, i, xt)
                end)
              else ();
              let fun loop_k k j =
                        if k < j then loop_k (k div 2) (j-k) else j+k
                  val j' = loop_k (n div 2) j
              in
                loop_i (i+1) j'
              end)
            in
              loop_i 1 1
            end;

            n
          end (* fft *)

    fun test out np = let
          val _ = (printi (out, np); say (out, "... "))
          val enp = R.fromInt np
          val npm = (np div 2) - 1
          val pxr = A.array (np+2, 0.0)
          val pxi = A.array (np+2, 0.0)
          val t = M.pi / enp
          val _ = A.update(pxr, 1, (enp - 1.0) * 0.5)
          val _ = A.update(pxi, 1, 0.0)
          val n2 = np  div  2
          val _ = A.update(pxr, n2+1, ~0.5)
          val _ = A.update(pxi, n2+1,  0.0)
          fun loop_i i = if i > npm then () else
            let val j = np - i
                val _ = A.update(pxr, i+1, ~0.5)
                val _ = A.update(pxr, j+1, ~0.5)
                val z = t * R.fromInt i
                val y = ~0.5*(M.cos(z)/M.sin(z))
                val _ = A.update(pxi, i+1,  y)
                val _ = A.update(pxi, j+1, ~y)
            in
              loop_i (i+1)
            end
          val _ = loop_i 1
    (***
          val _ = print "\n"
          fun loop_i i = if i > 15 then () else
            (print i; print "\t";
             print (sub(pxr, i+1)); print "\t";
             print (sub(pxi, i+1)); print "\n"; loop_i (i+1))
          val _ = loop_i 0
    ***)
          val _ = fft out pxr pxi np
    (***
          fun loop_i i = if i > 15 then () else
            (print i; print "\t";
             print (sub(pxr, i+1)); print "\t";
             print (sub(pxi, i+1)); print "\n"; loop_i (i+1))
          val _ = loop_i 0
    ***)
          fun loop_i i zr zi kr ki = if i >= np then (zr,zi) else
            let val a = R.abs(A.sub(pxr, i+1) - R.fromInt i)
                val (zr, kr) =
                  if zr < a then (a, i) else (zr, kr)
                val a = R.abs(A.sub(pxi, i+1))
                val (zi, ki) =
                  if zi < a then (a, i) else (zi, ki)
            in
              loop_i (i+1) zr zi kr ki
            end
          val (zr, zi) = loop_i 0 0.0 0.0 0 0
          val zm = if R.abs zr < R.abs zi then zi else zr
          in
            printr (out, zm); say(out, "\n")
          end (* test *)

    val N = 21

    fun loop_np out i np = if i > N
          then ()
          else (test out np; loop_np out (i+1) (np*2))

    fun doit () = loop_np NONE 1 16

    fun testit outstream = loop_np (SOME outstream) 1 16

  end;
