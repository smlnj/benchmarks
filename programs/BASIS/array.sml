(* array.sml
 *
 * COPYRIGHT (c) 2015 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Array : ARRAY =
  struct

    structure U = Unsafe

    type 'a array = 'a Array.array
    type 'a vector = 'a Array.vector

    (* fast add/subtract avoiding the overflow test *)
    infix -- ++
    fun x -- y = Word.toIntX(Word.fromInt x - Word.fromInt y)
    fun x ++ y = Word.toIntX(Word.fromInt x + Word.fromInt y)

    fun ltu (i, n) = Word.<(Word.fromInt i, Word.fromInt n)

    val maxLen = Array.maxLen

    fun array (0, _) = U.Array.create0()
      | array (n, x) = if ltu(maxLen, n)
          then raise General.Size
          else U.Array.create(n, x)

    fun fromList [] = U.Array.create0()
      | fromList (l as (first::rest)) =
          let fun len(_::_::r, i) = len(r, i ++ 2)
                | len([x], i) = i ++ 1
                | len([], i) = i
              val n = len(l, 0)
              val a = array(n, first)
              fun fill (i, []) = a
                | fill (i, x::r) = (U.Array.update(a, i, x); fill(i ++ 1, r))
           in fill(1, rest)
          end

    fun tabulate (0, _) = U.Array.create0()
      | tabulate (n, f : int -> 'a) : 'a array =
          let val a = array(n, f 0)
              fun tab i =
                if (i < n) then (U.Array.update(a, i, f i);
				 tab(i ++ 1))
                else a
           in tab 1
          end


    val length : 'a array -> int = Array.length
    val sub : 'a array * int -> 'a = Array.sub
    val update : 'a array * int * 'a -> unit = Array.update

    val usub = U.Array.sub
    val uupd = U.Array.update
    val vusub = U.Vector.sub
    val vlength = Vector.length

    fun copy { src, dst, di } = let
	val sl = length src
	val de = sl + di
	fun copyDn (s,  d) =
	    if s < 0 then () else (uupd (dst, d, usub (src, s));
				   copyDn (s -- 1, d -- 1))
    in
	if di < 0 orelse de > length dst then raise Subscript
	else
	    copyDn (sl -- 1, de -- 1)
    end

    fun copyVec { src, dst, di } = let
	val sl = vlength src
	val de = sl + di
	fun copyDn (s, d) =
	    if s < 0 then () else (uupd (dst, d, vusub (src, s));
				   copyDn (s -- 1, d -- 1))
    in
	if di < 0 orelse de > length dst then raise Subscript
	else copyDn (sl -- 1, de -- 1)
    end

    fun appi f arr = let
	val len = length arr
	fun app i =
	    if i < len then (f (i, usub (arr, i)); app (i ++ 1))
	    else ()
    in
	app 0
    end

    fun app f arr = let
	val len = length arr
	fun app i =
	    if i < len then (f (usub (arr, i)); app (i ++ 1))
	    else ()
    in
	app 0
    end

    fun modifyi f arr = let
	val len = length arr
	fun mdf i =
	    if i < len then (uupd (arr, i, f (i, usub (arr, i))); mdf (i ++ 1))
	    else ()
    in
	mdf 0
    end

    fun modify f arr = let
	val len = length arr
	fun mdf i =
	    if i < len then (uupd (arr, i, f (usub (arr, i))); mdf (i ++ 1))
	    else ()
    in
	mdf 0
    end

    fun foldli f init arr = let
	val len = length arr
	fun fold (i, a) =
	    if i < len then fold (i ++ 1, f (i, usub (arr, i), a)) else a
    in
	fold (0, init)
    end

    fun foldl f init arr = let
	  val len = length arr
	  fun fold (i, a) =
	      if i < len then fold (i ++ 1, f (usub (arr, i), a)) else a
    in
	fold (0, init)
    end

    fun foldri f init arr = let
	fun fold (i, a) =
	    if i < 0 then a else fold (i -- 1, f (i, usub (arr, i), a))
    in
	fold (length arr -- 1, init)
    end

    fun foldr f init arr = let
	fun fold (i, a) =
	    if i < 0 then a else fold (i -- 1, f (usub (arr, i), a))
    in
	fold (length arr -- 1, init)
    end

    fun findi p arr = let
	val len = length arr
	fun fnd i =
	    if i >= len then NONE
	    else let val x = usub (arr, i)
		 in
		     if p (i, x) then SOME (i, x) else fnd (i ++ 1)
		 end
    in
	fnd 0
    end

    fun find p arr = let
	val len = length arr
	fun fnd i =
	    if i >= len then NONE
	    else let val x = usub (arr, i)
		 in
		     if p x then SOME x else fnd (i ++ 1)
		 end
    in
	fnd 0
    end

    fun exists p arr = let
	val len = length arr
	fun ex i = i < len andalso (p (usub (arr, i)) orelse ex (i ++ 1))
    in
	ex 0
    end

    fun all p arr = let
	val len = length arr
	fun al i = i >= len orelse (p (usub (arr, i)) andalso al (i ++ 1))
    in
	al 0
    end

    fun collate c (a1, a2) = let
	val l1 = length a1
	val l2 = length a2
	val l12 = Int.min (l1, l2)
	fun coll i =
	    if i >= l12 then Int.compare (l1, l2)
	    else case c (usub (a1, i), usub (a2, i)) of
		     EQUAL => coll (i ++ 1)
		   | unequal => unequal
    in
	coll 0
    end

  (* added for Basis Library proposal 2015-003 *)
    fun toList arr = foldr op :: [] arr

    (* FIXME: this is inefficient (going through intermediate list) *)
    fun vector arr = Vector.fromList (toList arr)

  (* added for Basis Library proposal 2015-003 *)
    fun fromVector vec = let
	  val n = vlength vec
	  in
	    if (n = 0)
	      then U.Array.create0()
	      else let
		val arr = array(n, U.Vector.sub(vec, 0))
		fun fill i = if (i < n)
		      then (
			U.Array.update(arr, i, vusub(vec, i));
			fill (i ++ 1))
		      else arr
		in
		  fill 1
		end
	  end

  (* added for Basis Library proposal 2015-003 *)
    val toVector = vector

end (* structure Array *)
