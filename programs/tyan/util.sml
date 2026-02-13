(* util.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure Util = struct
    exception NotImplemented of string
    exception Impossible of string (* flag "impossible" condition  *)
    exception Illegal of string (* flag function use violating precondition *)

    fun error exn msg = raise (exn msg)
    fun notImplemented msg = error NotImplemented msg
    fun impossible msg = error Impossible msg
    fun illegal msg = error Illegal msg

    (* arr[i] := obj :: arr[i]; extend non-empty arr if necessary *)
    fun insert (obj,i,arr) = let
          val len = Array.length arr
          val res =  if i<len then (Array.update(arr,i,obj::Array.sub(arr,i)); arr)
             else let val arr' = Array.array(Int.max(i+1,len+len),[])
                      fun copy ~1 = (Array.update(arr',i,[obj]); arr')
                        | copy j = (Array.update(arr',j,Array.sub(arr,j));
                                    copy(j-1))
                      in copy(len-1) end
          in res
          end

    (* given compare and array a, return list of contents of a sorted in
     * ascending order, with duplicates stripped out; which copy of a duplicate
     * remains is random.  NOTE that a is modified.
     *)
    fun stripSort compare = fn a => let
          infix sub


          val op sub = Array.sub and update = Array.update
          fun swap (i,j) = let val ai = a sub i
                           in update(a,i,a sub j); update(a,j,ai) end
          (* sort all a[k], 0<=i<=k<j<=length a *)
          fun s (i,j,acc) = if i=j then acc else let
                val pivot = a sub ((i+j) div 2)
                fun partition (lo,k,hi) = if k=hi then (lo,hi) else
                      case compare (pivot,a sub k) of
                          LESS => (swap (lo,k); partition (lo+1,k+1,hi))
                        | EQUAL => partition (lo,k+1,hi)
                        | GREATER => (swap (k,hi-1); partition (lo,k,hi-1))
                val (lo,hi) = partition (i,i,j)
                in s(i,lo,pivot::s(hi,j,acc)) end
           val res = s(0,Array.length a,[])

          in
           res
          end
end
