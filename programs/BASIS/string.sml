(* string.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure String : STRING =
  struct

    structure V = Unsafe.CharVector

  (* fast add/subtract avoiding the overflow test *)
    infix -- ++
    fun x -- y = Word.toIntX(Word.fromInt x - Word.fromInt y)
    fun x ++ y = Word.toIntX(Word.fromInt x + Word.fromInt y)

    fun uLessThan (x, y) = Word.<(Word.fromInt x, Word.fromInt y)

    val unsafeSub = V.sub
    val unsafeUpdate = V.update
    val unsafeCreate = V.create

  (* list reverse *)
    fun listRev ([], l) = l
      | listRev (x::r, l) = listRev (r, x::l)

    type char = char
    type string = string

    val maxSize = CharVector.maxLen

  (* these functions are implemented in base/system/smlnj/init/pervasive.sml *)
    val size = CharVector.length
    val op ^ = op ^
    val concat = concat
    val implode = implode
    val explode = explode
    val substring = substring

  (* allocate an uninitialized string of given length *)
    fun create n = if (uLessThan(maxSize, n))
	  then raise General.Size
	  else unsafeCreate n

    val chars = let
          fun next i = if (i <= 255)
                then let
                  val s = unsafeCreate 1
                  in
                    unsafeUpdate(s, 0, chr i);  s :: next(i+1)
                  end
                else []
          in
            Vector.fromList(next 0)
          end

    fun unsafeSubstring (_, _, 0) = ""
      | unsafeSubstring (s, i, 1) =
	  Vector.sub (chars, ord (unsafeSub (s, i)))
      | unsafeSubstring (s, i, n) = let
	  val ss = unsafeCreate n
	  fun copy j = if (j = n)
		then ()
		else (unsafeUpdate(ss, j, unsafeSub(s, i+j)); copy(j+1))
	  in
	    copy 0; ss
	  end

  (* concatenate a pair of non-empty strings *)
    fun concat2 (x, y) = let
	  val xl = size x and yl = size y
	  val ss = create(xl+yl)
	  fun copyx n = if (n = xl)
		then ()
		else (unsafeUpdate(ss, n, unsafeSub(x, n)); copyx(n+1))
	  fun copyy n = if (n = yl)
		then ()
		else (unsafeUpdate(ss, xl+n, unsafeSub(y,n)); copyy(n+1))
	  in
	    copyx 0; copyy 0;
	    ss
	  end

  (* given a reverse order list of strings and a total length, return
   * the concatenation of the list.
   *)
    fun revConcat (0, _) = ""
      | revConcat (1, lst) = let
	  fun find ("" :: r) = find r
	    | find (s :: _) = s
	    | find _ = "" (** impossible **)
	  in
	    find lst
	  end
      | revConcat (totLen, lst) = let
	  val ss = create totLen
	  fun copy ([], _) = ()
	    | copy (s::r, i) = let
		val len = size s
		val i = i - len
		fun copy' j = if (j = len)
		      then ()
		      else (
			unsafeUpdate(ss, i+j, unsafeSub(s, j));
			copy'(j+1))
		in
		  copy' 0;
		  copy (r, i)
		end
	  in
	    copy (lst, totLen);  ss
	  end

  (* added for Basis Library proposal 2015-003 *)
    fun implodeRev [] = ""
      | implodeRev l = let
	  fun length l = let
		fun loop (n, []) = n
		  | loop (n, [_]) = n ++ 1
		  | loop (n, _ :: _ :: l) = loop (n ++ 2, l)
		in
		  loop (0, l)
		end
	  val n = length l
	  val s = create n
	  fun fill ([], _) = s
	    | fill (c::cs, i) = (
		unsafeUpdate(s, i, c);
		fill (cs, i -- 1))
	  in
	    fill (l, n -- 1)
	  end
  (* end Basis Library proposal 2015-003 *)

  (* convert a character into a single character string *)
    fun str (c : Char.char) : string = Vector.sub(chars, ord c)

  (* get a character from a string *)
    val sub : (string * int) -> char = CharVector.sub

    fun extract (v, base, optLen) = let
	  val len = size v
	  fun newVec n = let
		val newV = unsafeCreate n
		fun fill i = if (i < n)
		      then (unsafeUpdate(newV, i, unsafeSub(v, base ++ i)); fill(i ++ 1))
		      else ()
		in
		  fill 0; newV
		end
	  in
	    case (base, optLen)
	     of (0, NONE) => v
	      | (_, SOME 0) => if ((base < 0) orelse (len < base))
		    then raise General.Subscript
		    else ""
	      | (_, NONE) => if ((base < 0) orelse (len < base))
		      then raise General.Subscript
		    else if (base = len)
		      then ""
		      else newVec (len - base)
	      | (_, SOME 1) =>
		  if ((base < 0) orelse (len < (base ++ 1)))
		    then raise General.Subscript
		    else str(unsafeSub(v, base))
	      | (_, SOME n) =>
		  if ((base < 0) orelse (n < 0) orelse (len < (base ++ n)))
		    then raise General.Subscript
		    else newVec n
	    (* end case *)
	  end

  (* concatenate a list of strings, using the given separator string *)
    fun concatWith _ [] = ""
      | concatWith _ [x] = x
      | concatWith sep (h :: t) =
	  concat (listRev (foldl (fn (x, l) => x :: sep :: l) [h] t, []))

    fun map f vec = (case (size vec)
	   of 0 => ""
	    | len => let
		val newVec = unsafeCreate len
		fun mapf i = if (i < len)
		      then (unsafeUpdate(newVec, i, f(unsafeSub(vec, i))); mapf(i+1))
		      else ()
		in
		  mapf 0; newVec
		end
	  (* end case *))

  (* map a translation function across the characters of a string *)
    fun translate tr s = let
	  val stop = size s
	  fun mkList (i, totLen, lst) = if (i < stop)
		then let val s' = tr (unsafeSub (s, i))
		  in
		    mkList (i+1, totLen + size s', s' :: lst)
		  end
		else revConcat (totLen, lst)
          in
	    mkList (0, 0, [])
          end

  (* tokenize a string using the given predicate to define the delimiter
   * characters.
   *)
    fun tokens isDelim s = let
	  val n = size s
	  fun substr (i, j, l) = if (i = j)
		then l
		else unsafeSubstring(s, i, j -- i)::l
	  fun scanTok (i, j, toks) = if (j < n)
		  then if (isDelim (unsafeSub (s, j)))
		    then skipSep(j ++ 1, substr(i, j, toks))
		    else scanTok (i, j ++ 1, toks)
		  else substr(i, j, toks)
	  and skipSep (j, toks) = if (j < n)
		  then if (isDelim (unsafeSub (s, j)))
		    then skipSep(j ++ 1, toks)
		    else scanTok(j, j ++ 1, toks)
		  else toks
	  in
	    listRev (scanTok (0, 0, []), [])
	  end
    fun fields isDelim s = let
	  val n = size s
	  fun substr (i, j, l) = unsafeSubstring(s, i, j -- i)::l
	  fun scanTok (i, j, toks) = if (j < n)
		  then if (isDelim (unsafeSub (s, j)))
		    then scanTok (j+1, j+1, substr(i, j, toks))
		    else scanTok (i, j+1, toks)
		  else substr(i, j, toks)
	  in
	    listRev (scanTok (0, 0, []), [])
	  end

  (* String comparisons *)
    local
      fun isPrefix' (s1, s2, i2, n2) = let
            val n1 = size s1
            fun eq (i, j) =
                  (i >= n1)
                  orelse ((unsafeSub(s1, i) = unsafeSub(s2, j)) andalso eq(i+1, j+1))
            in
              (n2 >= n1) andalso eq (0, i2)
            end
    in
    fun isPrefix s1 s2 = isPrefix' (s1, s2, 0, size s2)
    fun isSuffix s1 s2 = let
          val sz2 = size s2
          in
            isPrefix' (s1, s2, sz2 - size s1, sz2)
          end
    end

    (* Knuth-Morris-Pratt String Matching
     *
     * val kmp : string -> string * int * int -> int option
     * Find the first string within the second, starting at and
     * ending before the given positions.
     * Return the starting position of the match
     * or the given ending position if there is no match. *)
    fun kmp pattern = let
          val psz = size pattern
          val next = Array.array (psz, ~1)
          fun pat x = unsafeSub (pattern, x)
          fun nxt x = Array.sub (next, x)
          fun setnxt (i, x) = Array.update (next, i, x)
          (* trying to fill next at position p (> 0) and higher;
           * invariants: x >= 0
           *             pattern[0..x) = pattern[p-x..p)
           *             for i in [0..p) :
           *                 pattern[i] <> pattern[next[i]]
           *                 pattern[0..next[i]) = pattern[i-next[i]..i) *)
          fun fill (p, x) = if p >= psz then ()
                            else if pat x = pat p then dnxt (p, nxt x, x + 1)
                            else dnxt (p, x, nxt x + 1)
          and dnxt (p, x, y) = (setnxt (p, x); fill (p + 1, y))
          (* Once next has been initialized, it serves the following purpose:
           * Suppose we are looking at text position t and pattern position
           * p.  This means that all pattern positions < p have already
           * matched the text positions that directly precede t.
           * Now, if the text[t] matches pattern[p], then we simply advance
           * both t and p and try again.
           * However, if the two do not match, then we simply
           * try t again, but this time with the pattern position
           * given by next[p].
           * Success is when p reaches the end of the pattern, failure is
           * when t reaches the end of the text without p having reached the
           * end of the pattern. *)
          fun search (text, start, tsz) = let
                fun txt x = unsafeSub (text, x)
                fun loop (p, t) =
                    if p >= psz then t - psz
                    else if t >= tsz then tsz
                    else if p < 0 orelse pat p = txt t then loop (p+1, t+1)
                    else loop (nxt p, t)
                in
                  loop (0, start)
                end
          in
            fill (1, 0); search
          end

    fun isSubstring s = let
          val stringsearch = kmp s
          fun search s' = let
              val epos = size s'
              in
                stringsearch (s', 0, epos) < epos
              end
          in
            search
          end

    fun collate cmpFn (s1, s2) = let
          val n1 = size s1
          val n2 = size s2
	  val (n, order) =
		if (n1 = n2) then (n1, EQUAL)
		else if (n1 < n2) then (n1, LESS)
		else (n2, GREATER)
	  fun cmp i = if (i = n)
		then order
		else let
		  val c1 = unsafeSub(s1, i)
		  val c2 = unsafeSub(s2, i)
		  in
		    case (cmpFn(c1, c2))
		     of EQUAL => cmp (i+1)
		      | order => order
		    (* end case *)
		  end
          in
            cmp 0
          end

    fun compare (a, b) = let
	  fun cmpFn (c1, c2) =
		if (c1 = c2) then EQUAL
		else if (Char.>(c1, c2)) then GREATER
		else LESS
          in
            collate cmpFn (a, b)
          end

    val scan = String.scan
    val fromString = StringCvt.scanString scan
    val toString = translate Char.toString
    val fromCString = String.fromCString
    val toCString = translate Char.toCString

  (* added for Basis Library proposal 2015-003 *)
    fun rev s = let
	  val n = size s
	  in
	    if (n < 2)
	      then s
	      else let
		val s' = unsafeCreate n
		fun fill i = if (i < n)
		      then (unsafeUpdate(s', i, unsafeSub(s, n--i--1)); fill(i++1))
		      else ()
		in
		  fill 0; s'
		end
	  end

    fun concatWithMap sep cvtFn = let
	  fun concat' [] = ""
	    | concat' [x] = cvtFn x
	    | concat' (x::xs) = let
		val sepLen = size sep
		fun cvt ([], strs, len) = let
		      val s' = unsafeCreate len
		      fun fill ([], _) = s'
			| fill (s::ss, i) = let
			    val n = size s
			    val i = i -- n
			    fun copy j = if j < n
				  then (unsafeUpdate(s', i++j, unsafeSub(s, j)); copy(j++1))
				  else fill (ss, i)
			    in
			      copy 0
			    end
		      in
			fill (strs, len)
		      end
		  | cvt (x::xs, strs, len) = let
		      val s = cvtFn x
		      val len = len ++ sepLen ++ size s
		      in
			if len > maxSize then raise General.Size else ();
			cvt (xs, s::sep::strs, len)
		      end
		val s = cvtFn x
		in
		  cvt (xs, [s], size s)
		end
	  in
	    concat'
	  end
  (* end Basis Library proposal 2015-003 *)

  (* String greater or equal *)
    fun sgtr (a, b) = let
	  val al = size a and bl = size b
	  val n = if (al < bl) then al else bl
	  fun cmp i = if (i = n)
		then (al > bl)
		else let
		  val ai = unsafeSub(a,i)
		  val bi = unsafeSub(b,i)
		  in
		    Char.>(ai, bi) orelse ((ai = bi) andalso cmp(i+1))
		  end
	  in
	    cmp 0
	  end

    fun op <= (a,b) = if sgtr(a,b) then false else true
    fun op < (a,b) = sgtr(b,a)
    fun op >= (a,b) = b <= a
    val op > = sgtr

  end

(* pervasive string functions *)
val concat = String.concat
val op ^ = String.^
val str = String.str
val size = String.size
val implode = String.implode
val explode = String.explode
val substring = String.substring
