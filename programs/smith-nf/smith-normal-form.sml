(* smith-normal-form.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure SmithNormalForm : sig

    val smithNormalForm : IntInf.int Matrix.matrix -> IntInf.int Matrix.matrix

  end = struct

val zero = IntInf.fromInt 0

fun smaller (a: IntInf.int, b: IntInf.int): bool =
   (not (a = zero))
   andalso (b = zero orelse IntInf.< (IntInf.abs a , IntInf.abs b))

fun smithNormalForm (mat: IntInf.int Matrix.matrix): IntInf.int Matrix.matrix =
   let val height = Matrix.height mat
      val width = Matrix.width mat
      val mat = Matrix.copy mat
      val range = Int.min (width, height)
      fun dd pos =
         let val matCol = Matrix.fetchCol (mat, pos)
            val matRow = Matrix.fetchRow (mat, pos)
(*
            val _ = print (concat["dd: pos = " , Int.toString pos, "\n"])
*)
            fun swapRowLoop (best, bestRow, bestCol, row) =
               if row >= height
                  then (Matrix.rowSwap (mat, pos, bestRow);
                        Matrix.colSwap (mat, pos, bestCol))
               else let val matRow = Matrix.fetchRow (mat, row)
                        fun swapColLoop (best, bestRow, bestCol, col) =
                           if col >= width
                              then swapRowLoop (best, bestRow, bestCol, row + 1)
                           else let val next = matRow col
                                in if smaller (next, best)
                                      then swapColLoop (next, row, col, col + 1)
                                   else swapColLoop (best, bestRow, bestCol, col + 1)
                                end
                    in swapColLoop (best, bestRow, bestCol, pos)
                    end
            fun rowLoop row =
               if row < height
                  then if (matCol row) = zero
                          then rowLoop (row + 1)
                       else (Matrix.rowOp (mat,
                                           pos,
                                           row,
                                           let val x = IntInf.~ (IntInf.quot(matCol row, matCol pos))
                                           in fn (lhs, rhs) => IntInf.+ (IntInf.* (lhs, x), rhs)
                                           end);
                             if (matCol row) = zero
                                then rowLoop (row + 1)
                             else hitPosAgain ())
               else let fun colLoop col =
                  if col < width
                     then if (matRow col) = zero
                             then colLoop (col + 1)
                          else (Matrix.colOp (mat,
                                              pos,
                                              col,
                                              let val x = IntInf.~ (IntInf.quot (matRow col, matRow pos))
                                              in fn (lhs, rhs) => IntInf.+ (IntInf.* (lhs, x), rhs)
                                              end);
                                if (matRow col) = zero
                                   then colLoop (col + 1)
                                else hitPosAgain ())
                  else ()
                    in colLoop (pos + 1)
                    end
            and hitPosAgain () = (swapRowLoop (zero, pos, pos, pos);
                                  rowLoop (pos + 1))
         in hitPosAgain ()
         end
      fun loop pos =
         if pos = range
            then mat
         else (dd pos;
               loop (pos + 1))
   in loop 0
   end


  end


