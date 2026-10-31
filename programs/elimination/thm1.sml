(*#line 0.0 "fol/Thm1.sml"*)
(* ========================================================================= *)
(* INTERFACE TO THE LCF-STYLE LOGICAL KERNEL, PLUS SOME DERIVED RULES        *)
(* Created by Joe Hurd, September 2001                                       *)
(* ========================================================================= *)

(*
app load ["Useful", "Term1", "Kernel1", "Match1"];
*)

(*
*)
structure Thm1 :> Thm1 =
struct

open Useful Term1 Kernel1 Match1;

infixr |-> ::> oo ##;

type subst        = Subst1.subst;
val |<>|          = Subst1.|<>|;
val op ::>        = Subst1.::>;
val term_subst    = Subst1.term_subst;
val formula_subst = Subst1.formula_subst;
val pp_subst      = Subst1.pp_subst;

(* ------------------------------------------------------------------------- *)
(* Annotated primitive inferences.                                           *)
(* ------------------------------------------------------------------------- *)

datatype inference' =
  Axiom'    of formula list
| Refl'     of term
| Assume'   of formula
| Inst'     of subst * thm
| Factor'   of thm
| Resolve'  of formula * thm * thm
| Equality' of formula * int list * term * bool * thm

fun primitive_inference (Axiom'    cl              ) = AXIOM cl
  | primitive_inference (Refl'     tm              ) = REFL tm
  | primitive_inference (Assume'   l               ) = ASSUME l
  | primitive_inference (Inst'     (s, th)         ) = INST s th
  | primitive_inference (Factor'   th              ) = FACTOR th
  | primitive_inference (Resolve'  (l, th1, th2)   ) = RESOLVE l th1 th2
  | primitive_inference (Equality' (l, p, t, s, th)) = EQUALITY l p t s th;

val clause = fst o dest_thm;

(* ------------------------------------------------------------------------- *)
(* Pretty-printing of theorems                                               *)
(* ------------------------------------------------------------------------- *)

fun pp_thm pp th =
  (PP.begin_block pp PP.INCONSISTENT 3;
   PP.add_string pp "|- ";
   pp_formula pp (list_mk_disj (clause th));
   PP.end_block pp);

local
  fun inf_to_string Axiom    = "Axiom"
    | inf_to_string Refl     = "Refl"
    | inf_to_string Assume   = "Assume"
    | inf_to_string Inst     = "Inst"
    | inf_to_string Factor   = "Factor"
    | inf_to_string Resolve  = "Resolve"
    | inf_to_string Equality = "Equality";
in
  val pp_inference = pp_map inf_to_string pp_string;
end;

local
  fun pp_inf (Axiom'    a) = (Axiom,   C (pp_list pp_formula)                 a)
    | pp_inf (Refl'     a) = (Refl,    C pp_term                              a)
    | pp_inf (Assume'   a) = (Assume,  C pp_formula                           a)
    | pp_inf (Inst'     a) = (Inst,    C (pp_pair pp_subst pp_thm)            a)
    | pp_inf (Factor'   a) = (Factor,  C pp_thm                               a)
    | pp_inf (Resolve'  a) = (Resolve, C (pp_triple pp_formula pp_thm pp_thm) a)
    | pp_inf (Equality' (lit, p, r, lr, th)) =
    (Equality,
     C (pp_record [("lit",  unit_pp pp_formula lit),
                   ("path", unit_pp (pp_list pp_int) p),
                   ("res",  unit_pp pp_term r),
                   ("lr",   unit_pp pp_bool lr),
                   ("thm",  unit_pp pp_thm th)]) ());
in
  fun pp_inference' pp inf =
  let
    open PP
    val (i, ppf) = pp_inf inf
  in
    (begin_block pp INCONSISTENT 0;
     pp_inference pp i;
     add_break pp (1, 0);
     ppf pp;
     end_block pp)
  end;
end;

val pp_proof = pp_list (pp_pair pp_thm pp_inference');

(* Purely functional pretty-printing *)

fun thm_to_string'       len = PP.pp_to_string len pp_thm;
fun inference_to_string' len = PP.pp_to_string len pp_inference';

(* Pretty-printing using !LINE_LENGTH *)

fun thm_to_string       th  = thm_to_string'       (!LINE_LENGTH) th;
fun inference_to_string inf = inference_to_string' (!LINE_LENGTH) inf;

(* ------------------------------------------------------------------------- *)
(* A total comparison function for theorems.                                 *)
(* ------------------------------------------------------------------------- *)

local
  fun cmp Axiom    Axiom    = EQUAL
    | cmp Axiom    _        = LESS
    | cmp _        Axiom    = GREATER
    | cmp Refl     Refl     = EQUAL
    | cmp Refl     _        = LESS
    | cmp _        Refl     = GREATER
    | cmp Assume   Assume   = EQUAL
    | cmp Assume   _        = LESS
    | cmp _        Assume   = GREATER
    | cmp Inst     Inst     = EQUAL
    | cmp Inst     _        = LESS
    | cmp _        Inst     = GREATER
    | cmp Factor   Factor   = EQUAL
    | cmp Factor   _        = LESS
    | cmp _        Factor   = GREATER
    | cmp Resolve  Resolve  = EQUAL
    | cmp Resolve  Equality = LESS
    | cmp Equality Resolve  = GREATER
    | cmp Equality Equality = EQUAL;

  fun cm [] = EQUAL
    | cm ((th1, th2) :: l) =
    let
      val (l1, (p1, ths1)) = dest_thm th1
      val (l2, (p2, ths2)) = dest_thm th2
    in
      case Int.compare (length l1, length l2) of EQUAL
        => (case lex_compare formula_compare (zip l1 l2) of EQUAL
              => (case cmp p1 p2 of EQUAL
                    => cm (zip ths1 ths2 @ l)
                  | x => x)
            | x => x)
      | x => x
    end
in
  val thm_compare = cm o wrap;
end;

(* ------------------------------------------------------------------------- *)
(* Reconstructing proofs.                                                    *)
(* ------------------------------------------------------------------------- *)

fun reconstruct_resolvant cl1 cl2 (cl1', cl2') =
  case (subtract (setify cl1) cl1', subtract (setify cl2) cl2') of
    (_ :: _ :: _, _) => NONE
  | (_, _ :: _ :: _) => NONE
  | ([l], []) => SOME l
  | ([], [l']) => SOME (negate l')
  | ([l], [l']) => if negate l = l' then SOME l else NONE
  | ([], []) => NONE;

fun reconstruct_equality l r =
  let
    fun recon_fn p (f, args) (f', args') rest =
      recon_tm
      (if f <> f' orelse length args <> length args' then rest
       else map (C cons p ## I) (enumerate 0 (zip args args')) @ rest)
    and recon_tm [] = NONE
      | recon_tm ((p, (tm, tm')) :: rest) =
      if tm = l andalso tm' = r then SOME (rev p)
      else
        case (tm, tm') of (Fn a, Fn a') => recon_fn p a a' rest
        | _ => recon_tm rest

    fun recon_lit (Not a) (Not a') = recon_lit a a'
      | recon_lit (Atom a) (Atom a') =
      if l <> r andalso a = a' then NONE else recon_tm [([], (a, a'))]
      | recon_lit _ _ = NONE
  in
    fn (lit, lit') =>
    case recon_lit lit lit' of SOME p => SOME (lit, p) | NONE => NONE
  end;

fun reconstruct (cl, (Axiom, [])) = Axiom' cl
  | reconstruct ([lit], (Refl, [])) = Refl' (lhs lit)
  | reconstruct ([fm, _], (Assume, [])) = Assume' fm
  | reconstruct (cl, (Inst, [th])) =
  Inst' (matchl_literals |<>| (zip (clause th) cl), th)
  | reconstruct (_, (Factor, [th])) = Factor' th
  | reconstruct (cl, (Resolve, [th1, th2])) =
  let
    val f = reconstruct_resolvant (clause th1) (clause th2)
    val l =
      case f (cl, cl) of SOME l => l
      | NONE =>
        case first f (List.tabulate (length cl, split cl)) of SOME l => l
        | NONE => raise BUG "inference" "couldn't reconstruct resolvant"
  in
    Resolve' (l, th1, th2)
  end
  | reconstruct (Not fm :: cl, (Equality, [th])) =
  let
    val (tm1, tm2) = dest_eq fm
  in
    case first (reconstruct_equality tm1 tm2) (zip (clause th) cl) of
      SOME (l, p) => Equality' (l, p, tm2, true, th)
    | NONE =>
      case first (reconstruct_equality tm2 tm1) (zip (clause th) cl) of
        SOME (l, p) => Equality' (l, p, tm1, false, th)
      | NONE => raise BUG "inference" "couldn't reconstruct equality step"
  end
  | reconstruct _ = raise BUG "inference" "malformed inference";

fun inference th =
  let
    val i = reconstruct (dest_thm th)
    val _ =
      (primitive_inference i = th) orelse
      raise BUG "inference"
      ("failed:\nth = " ^ thm_to_string th ^ "\ninf = " ^ inference_to_string i
       ^ "\ninf_th = " ^ thm_to_string (primitive_inference i))
  in
    i
  end;

local
  val empty = (Binarymap.mkDict thm_compare, []);
  fun contains (m, _) th = Option.isSome (Binarymap.peek (m, th));
  fun add th (m, p) = (Binarymap.insert (m, th, ()), (th, inference th) :: p);
  val finalize = snd;

  fun reduce (th, pf) =
    if contains pf th then pf
    else add th (foldl reduce pf (snd (snd (dest_thm th))));
in
  fun proof th = finalize (reduce (th, empty));
end;

(* ------------------------------------------------------------------------- *)
(* Contradictions and unit clauses.                                          *)
(* ------------------------------------------------------------------------- *)

val is_contradiction = null o clause;

fun dest_unit th =
  case clause th of [lit] => lit | _ => raise ERR "dest_unit" "not a unit";

val is_unit = can dest_unit;

(* ------------------------------------------------------------------------- *)
(* Derived rules                                                             *)
(* ------------------------------------------------------------------------- *)

fun CONTR lit th = RESOLVE (negate lit) (ASSUME lit) th;

fun WEAKEN lits th = foldl (uncurry CONTR) th (rev lits);

fun FRESH_VARSL ths =
  let
    val fvs = FVL (List.concat (map clause ths))
    val vvs = new_vars (length fvs)
    val sub = Subst1.from_maplets (zipwith (curry op |->) fvs vvs)
  in
    map (INST sub) ths
  end;

val FRESH_VARS = unwrap o FRESH_VARSL o wrap;

fun UNIT_SQUASH th =
  let
    fun squash env (x :: (xs as y :: _)) = squash (unify_literals env x y) xs
      | squash env _ = env
  in
    FACTOR (INST (squash |<>| (clause th)) th)
  end;

val REFLEXIVITY = REFL (Var "x");

val SYMMETRY =
  EQUALITY (mk_eq (Var "x", Var "x")) [0] (Var "y") true REFLEXIVITY;

val TRANSITIVITY =
  EQUALITY (mk_eq (Var "y", Var "z")) [0] (Var "x") false
  (ASSUME (Not (mk_eq (Var "y", Var "z"))));

fun FUN_CONGRUENCE (function, arity) =
  let
    val xs = List.tabulate (arity, fn i => Var ("x" ^ int_to_string i))
    val ys = List.tabulate (arity, fn i => Var ("y" ^ int_to_string i))
    fun f (i, th) =
      EQUALITY (List.last (clause th)) [1,i] (List.nth (ys, i)) true th
    val refl = INST (("x" |-> Fn (function, xs)) ::> |<>|) REFLEXIVITY
  in
    foldl f refl (rev (interval 0 arity))
  end;

fun REL_CONGRUENCE (relation, arity) =
  let
    val xs = List.tabulate (arity, fn i => Var ("x" ^ int_to_string i))
    val ys = List.tabulate (arity, fn i => Var ("y" ^ int_to_string i))
    fun f (i, th) =
      EQUALITY (List.last (clause th)) [i] (List.nth (ys, i)) true th
    val refl = ASSUME (Not (Atom (Fn (relation, xs))))
  in
    foldl f refl (rev (interval 0 arity))
  end;

end
