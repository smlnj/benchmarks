(* char-vector.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure CharVector : MONO_VECTOR =
  struct

    structure V = Unsafe.CharVector

    (* fast add/subtract avoiding the overflow test *)
    infix -- ++
    fun x -- y = Word.toIntX(Word.fromInt x - Word.fromInt y)
    fun x ++ y = Word.toIntX(Word.fromInt x + Word.fromInt y)

    fun uLessThan (x, y) = Word.<(Word.fromInt x, Word.fromInt y)

  (* unchecked access operations *)
    val usub = V.sub
    val uupd = V.update

    type vector = V.vector
    type elem = V.elem

    val maxLen = CharVector.maxLen

    val vector0 : vector = V.create 0

    fun createVec n = if uLessThan(maxLen, n)
	  then raise Size
	  else V.create n

    fun fromList [] = vector0
      | fromList vl = let
          val len = let
                fun lp ([], n) = n
                  | lp (_::r, n) = lp (r, n ++ 1)
                in
                  lp (vl, 0)
                end
	  val v = createVec len
	  fun copy ([], _) = ()
	    | copy (b::r, i) = (uupd(v, i, b); copy(r, i++1))
	  in
	    copy (vl, 0); v
	  end

    fun tabulate (0, _) = vector0
      | tabulate (n, f) = let
	  val ss = createVec n
	  fun fill i =
	      if i < n then (uupd (ss, i, f i); fill (i ++ 1))
	      else ()
	  in
	    fill 0; ss
	  end

    val length = CharVector.length
    val sub = CharVector.sub

    fun concat [] = vector0
      | concat [s] = s
      | concat (sl : vector list) = let
        (* compute total length of the result string *)
          fun len (i, []) = i
            | len (i, s::rest) = len(i+length s, rest)
          in
            case len (0, sl)
             of 0 => vector0
              | 1 => let
                  fun find (v :: r) = if length v = 0 then find r else v
                    | find _ = vector0 (** impossible **)
                  in
                    find sl
                  end
              | totLen => let
                  val v = createVec totLen
                  fun copy ([], _) = ()
                    | copy (s::r, i) = let
                        val len = length s
                        fun copy' j =
                            if (j = len) then ()
                            else (uupd(v, i++j, usub(s, j)); copy'(j++1))
                        in
                          copy' 0;
                          copy (r, i++len)
                        end
                  in
                    copy (sl, 0);
                    v
                  end
            (* end case *)
          end (* concat *)

    fun appi f vec = let
	val len = length vec
	fun app i =
	    if i >= len then () else (f (i, usub (vec, i)); app (i ++ 1))
        in
          app 0
        end

    fun app f vec = let
	val len = length vec
	fun app i =
	    if i >= len then () else (f (usub (vec, i)); app (i ++ 1))
        in
          app 0
        end

    val update = CharVector.update

    fun mapi f vec = tabulate (length vec, fn i => f (i, usub (vec, i)))

    fun map f vec = (case (length vec)
	   of 0 => vector0
	    | len => let
		val newVec = V.create len
		fun mapf i = if (i < len)
		      then (uupd(newVec, i, f(usub(vec, i))); mapf(i+1))
		      else ()
		in
		  mapf 0; newVec
		end
	  (* end case *))

    fun foldli f init vec = let
	val len = length vec
	fun fold (i, a) =
	    if i >= len then a else fold (i ++ 1, f (i, usub (vec, i), a))
        in
          fold (0, init)
        end

    fun foldl f init vec = let
	val len = length vec
	fun fold (i, a) =
	    if i >= len then a else fold (i ++ 1, f (usub (vec, i), a))
        in
          fold (0, init)
        end

    fun foldri f init vec = let
	fun fold (i, a) =
	    if i < 0 then a else fold (i --1, f (i, usub (vec, i), a))
        in
	  fold (length vec -- 1, init)
        end

    fun foldr f init vec = let
	fun fold (i, a) =
	    if i < 0 then a else fold (i --1, f (usub (vec, i), a))
        in
	  fold (length vec -- 1, init)
        end

    fun findi p vec = let
	val len = length vec
	fun fnd i =
	    if i >= len then NONE
	    else let val x = usub (vec, i)
		 in
		     if p (i, x) then SOME (i, x) else fnd (i ++ 1)
		 end
        in
          fnd 0
        end

    fun find p vec = let
	val len = length vec
	fun fnd i =
	    if i >= len then NONE
	    else let val x = usub (vec, i)
		 in
		     if p x then SOME x else fnd (i ++ 1)
		 end
        in
          fnd 0
        end

    fun exists p vec = let
	val len = length vec
	fun ex i = i < len andalso (p (usub (vec, i)) orelse ex (i ++ 1))
        in
          ex 0
        end

    fun all p vec = let
	val len = length vec
	fun al i = i >= len orelse (p (usub (vec, i)) andalso al (i ++ 1))
        in
          al 0
        end

    fun collate c (v1, v2) = let
	val l1 = length v1
	val l2 = length v2
	val l12 = Int.min (l1, l2)
	fun col i =
	    if i >= l12 then Int.compare (l1, l2)
	    else (case c (usub (v1, i), usub (v2, i))
		  of EQUAL => col (i ++ 1)
		   | unequal => unequal)
        in
          col 0
        end

    (* added for Basis Library proposal 2015-003 *)
    local
    (* utility function for extracting the elements of a vector as a list *)
      fun getList (_, 0, l) = l
	| getList (vec, i, l) = let val i = i -- 1
	    in
	      getList (vec, i, usub(vec, i) :: l)
	    end
    in

    fun toList vec = let
	  val n = length vec
	  in
	    getList (vec, n, [])
	  end

    fun append (vec, x) = let
	  val n = length vec
	  val n' = n ++ 1
	  val ss = createVec n'
	  fun fill i = if i < n
		then (uupd (ss, i, usub(vec, i)); fill (i ++ 1))
	        else ()
	  in
	    fill 0; uupd (ss, n, x);
	    ss
	  end

    fun prepend (x, vec) = let
	  val n = length vec
	  val n' = n ++ 1
	  val ss = createVec n'
	  fun fill i = if i < n
		then (uupd (ss, i ++ 1, usub(vec, i)); fill (i ++ 1))
	        else ()
	  in
	    uupd (ss, 0, x); fill 0;
	    ss
	  end

    end (* local *)

  end
