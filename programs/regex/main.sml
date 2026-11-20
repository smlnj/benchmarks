(* main.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure Main (*: BMARK*) =
  struct

    val name = "regex"

    datatype re
      = EMPTY		(* empty set *)
      | NULL		(* empty string *)
      | CHR of char
      | ANY             (* match any character *)
      | ALT of re * re	(* alternation *)
      | SEQ of re * re	(* concatenation *)
      | REP of re	(* Kleene closure *)
      | NOT of re       (* set complement *)
      | AND of re * re  (* set intersection *)

    fun toS re = let
          fun atomic EMPTY = true
            | atomic NULL = true
            | atomic (CHR c) = true
            | atomic ANY = true
            | atomic _ = false
          fun toS' EMPTY = "∅"
            | toS' NULL = "ε"
            | toS' (CHR c) = str c
            | toS' ANY = "."
            | toS' (ALT(re1, re2)) = concat[toS' re1, "|", toS' re2]
            | toS' (SEQ(re1, re2)) = let
                fun needsParen (SEQ _) = false
                  | needsParen re = not(atomic re)
                in
                  paren needsParen re1 ^ paren needsParen re2
                end
            | toS' (REP re) = concat[paren atomic re, "*"]
            | toS' (NOT re) = concat["¬", paren atomic re]
            | toS' (AND(re1, re2)) = let
                fun needsParen (AND _) = false
                  | needsParen (SEQ _) = false
                  | needsParen re = not(atomic re)
                in
                  concat[paren needsParen re1, "&", paren needsParen re2]
                end
          and paren pred re = if pred re
                then concat["(", toS' re, ")"]
                else toS' re
          in
            toS' re
          end

    (* smart constructors *)
    fun seq (EMPTY, _) = EMPTY
      | seq (_, EMPTY) = EMPTY
      | seq (NULL, re) = re
      | seq (re, NULL) = re
      | seq (SEQ(re1, re2), re3) = seq(re1, seq(re2, re3))
      | seq (re1, re2) = SEQ(re1, re2)

    fun alt (EMPTY, re) = re
      | alt (re, EMPTY) = re
      | alt (ALT(re1, re2), re3) = alt(re1, alt(re2, re3))
      | alt (re1, re2) = if (re1 = re2) then re1 else ALT(re1, re2)

    fun rep EMPTY = EMPTY
      | rep NULL = NULL
      | rep re = REP re

    fun not' EMPTY = REP ANY
      | not' (NOT re) = re
      | not' re = NOT re

    fun and' (EMPTY, _) = EMPTY
      | and' (_, EMPTY) = EMPTY
      | and' (AND(re1, re2), re3) = and'(re1, and'(re2, re3))
      | and' (re1, re2) = if (re1 = re2) then re1 else AND(re1, re2)

    (* helpers for constructing REs *)

    fun string s = (case rev(explode s)
           of [] => NULL
            | c::rest => List.foldl (fn (c, re) => SEQ(CHR c, re)) (CHR c) rest
          (* end case *))

    fun oneof s = (case rev(explode s)
           of [] => EMPTY
            | c::rest => List.foldl (fn (c, re) => ALT(CHR c, re)) (CHR c) rest
          (* end case *))

    fun seqFromList [] = NULL
      | seqFromList [re] = re
      | seqFromList (re::rest) = seq(re, seqFromList rest)

    fun altFromList [] = EMPTY
      | altFromList [re] = re
      | altFromList (re::rest) = alt(re, altFromList rest)

    (* matching *)

    (* nullable : re -> re; returns NULL if NULL \in L[[re]] and EMPTY otherwise *)
    fun nullable EMPTY = EMPTY
      | nullable NULL = NULL
      | nullable (CHR _) = EMPTY
      | nullable ANY = EMPTY
      | nullable (SEQ(re1, re2)) = seq(nullable re1, nullable re2)
      | nullable (ALT(re1, re2)) = alt(nullable re1, nullable re2)
      | nullable (REP _) = NULL
      | nullable (NOT re) = (case nullable re of NULL => EMPTY | _ => NULL)
      | nullable (AND(re1, re2)) = and'(nullable re1, nullable re2)

    fun isNullable re = (nullable re = NULL)

    (* derivative : char -> re -> re *)
    fun derivative c = let
	  fun d_c EMPTY = EMPTY
	    | d_c NULL = EMPTY
	    | d_c (CHR c') = if (c = c') then NULL else EMPTY
            | d_c ANY = NULL
	    | d_c (SEQ(re1, re2)) = alt(seq(d_c re1, re2), seq(nullable re1, d_c re2))
	    | d_c (ALT(re1, re2)) = alt(d_c re1, d_c re2)
	    | d_c (REP re) = seq(d_c re, REP re)
            | d_c (NOT re) = not'(d_c re)
            | d_c (AND(re1, re2)) = and'(d_c re1, d_c re2)
	  in
	    fn re => (print(concat["∂", str c, " (", toS re, ")\n"]); d_c re)
	  end

    fun match (EMPTY, _) = false
      | match (NOT re, []) = not(isNullable re)
      | match (re, []) = isNullable re
      | match (re, c::rest) = match(derivative c re, rest)

    (* check on "a*b" *)
    fun testit outS = if match (seq(rep(CHR #"a"), CHR #"b"), explode "aab")
	  then TextIO.output(outS, "ok\n")
	  else TextIO.output(outS, "fail\n")

    (* construct RE for L2 = { s#w#s'$w | w ∊ {0,1}^2 and s,s' ∊ {0,1,#}* } *)
    local
      val Z = CHR #"0"
      val O = CHR #"1"
      val D = CHR #"$"
      val H = CHR #"#"
      val ZO = alt(Z, O)
      val ZOHstar = rep(oneof "01#")
    in
    val L2a = seqFromList[rep(NOT D), D, rep(NOT D)]
    val L2b = seqFromList[
            ZOHstar, H,
            and'(
              (* (0|1)0#(0|1|#)*$(0|1)0 | (0|1)1#(0|1|#)*$(0|1)1 *)
              alt(
                seqFromList[ZO, Z, H, ZOHstar, D, ZO, O],
                seqFromList[ZO, O, H, ZOHstar, D, ZO, O]),
              (* 0(0|1)#(0|1|#)*$0(0|1) | 1(0|1)#(0|1|#)*$1(0|1) *)
              alt(
                seqFromList[Z, ZO, H, ZOHstar, D, Z, ZO],
                seqFromList[O, ZO, H, ZOHstar, D, O, ZO]))
          ]
    val L2 = and'(L2a, L2b)
    end (* local *)

    val input1 = explode "00011010#00#1110#01###11110000$10"
    val input2 = explode "0#01#0$10"

    fun once () = ignore (match (L2, input1))
          
    fun doit () = once()

  end

