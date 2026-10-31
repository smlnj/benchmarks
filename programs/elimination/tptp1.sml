(*#line 0.0 "fol/Tptp1.sml"*)
(* ========================================================================= *)
(* INTERFACE TO TPTP PROBLEM FILES                                           *)
(* Created by Joe Hurd, December 2001                                        *)
(* ========================================================================= *)

(*
app load ["Stream", "Useful", "Parser", "Term1"];
*)

(*
*)
structure Tptp1 :> Tptp1 =
struct

open Parser Useful Term1;

infixr 9 >>++;
infixr 8 ++;
infixr 7 >>;
infixr 6 ||;
infix |->;

structure S = Stream;

(* ------------------------------------------------------------------------- *)
(* Abbreviating relation and function names in TPTP problems.                *)
(* ------------------------------------------------------------------------- *)

type rename = {tptp : string, fol : string, arity : int};

val renaming : rename list ref = ref [{tptp = "equal", fol = "=", arity = 2}];

(* ------------------------------------------------------------------------- *)
(* Parsing: pass in a filename.                                              *)
(* ------------------------------------------------------------------------- *)

val comment = equal #"%" o hd o explode;

val input_lines = S.filter (non comment) o S.from_textfile;

val input_chars = S.flatten o S.map (S.from_list o explode);

datatype tok_type = Lower | Upper | Symbol | Punct;

val lexer =
  (many (some space) ++
   (((some lower || some digit) ++ many (some alphanum) >>
     (fn (a, b) => (Lower, implode (a :: b)))) ||
    (some upper ++ many (some alphanum) >>
     (fn (a, b) => (Upper, implode (a :: b)))) ||
    (atleastone (some symbol) >> (fn l => (Symbol, implode l))) ||
    (some punct >> (fn c => (Punct, str c))))) >> snd;

val lex = many lexer ++ (many (some space) ++ finished) >> fst;

val input_toks = S.from_list o fst o lex;

fun Var' "T" = Var "T'"
  | Var' "F" = Var "F'"
  | Var' v = Var (if !var_string v then v else "v_" ^ v);

local
  fun verify (f, a) =
    (if !var_string f then (if null a then "c_" else "f_") ^ f else f, a);
  fun mapped (f, a) (m : rename list) =
    let
       fun g {tptp, arity, fol = _} = tptp = f andalso arity = length a
    in case List.find g m of SOME {fol, ...} => (fol, a) | NONE => verify (f, a)
    end;
in
  fun Fn'   A = Fn   (mapped A (!renaming));
end;

fun term_parser input =
  ((some (equal Upper o fst) >> (Var' o snd)) ||
   ((some (equal Lower o fst) >> snd) ++
    (optional
     (exact (Punct, "(") ++ term_parser ++
      many ((exact (Punct, ",") ++ term_parser) >> snd) ++
      exact (Punct, ")")) >>
     (fn SOME (_, (t, (ts, _))) => t :: ts | NONE => [])) >>
    Fn')) input;

val literal_parser =
  ((exact (Symbol, "++") >> K true || exact (Symbol, "--") >> K false) ++
   term_parser) >>
  (fn (s, t) => mk_literal (s, Atom (case t of Var v => Fn (v, []) | _ => t)));

val clause_parser =
  (exact (Lower, "input_clause") ++ exact (Punct, "(") ++ any ++
   exact (Punct, ",") ++ any ++ exact (Punct, ",") ++ exact (Punct, "[") ++
   literal_parser ++ many ((exact (Punct, ",") ++ literal_parser) >> snd) ++
   exact (Punct, "]") ++ exact (Punct, ")") ++ exact (Punct, ".")) >>
  (fn (_, (_, (name, (_, (typ, (_, (_, (l, (ls, _))))))))) =>
   (snd name, snd typ, l :: ls));

val cnf_parser = fst o ((many clause_parser ++ finished) >> fst);

local
  fun cycle _ _ ([], _) = raise BUG "cycle" ""
    | cycle f v (h :: t, avoid) =
    let val h' = f h avoid in (h', (t @ [h], h' :: avoid)) end;
in
  fun generalize_clause fm =
    let
      open Subst1
      val vars = FV fm
      val nvars = length vars
      val var_fn = if nvars <= 15 then variant else variant_num
      val news =
        if nvars = 6 then ["x", "y", "z", "x'", "y'", "z'"]
        else fst (maps (cycle var_fn) vars (["x", "y", "z", "v", "w"], []))
      val sub = from_maplets (zipwith (fn v => fn x => v |-> Var x) vars news)
    in
      generalize (formula_subst sub fm)
    end;
end;

val input_cnf =
  (fn (a, b) => Imp (a, Imp (b, False))) o
  Df (list_mk_conj o map (generalize_clause o list_mk_disj o #3)) o
  List.partition (not o equal "conjecture" o #2) o cnf_parser;

val parse_cnf = input_cnf o input_toks o input_chars o input_lines;

end
