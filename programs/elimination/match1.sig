(*#line 0.0 "fol/Match1.sig"*)
(* ========================================================================= *)
(* MATCHING AND UNIFICATION                                                  *)
(* Created by Joe Hurd, September 2001                                       *)
(* ========================================================================= *)

signature Match1 =
sig

type term    = Term1.term
type formula = Term1.formula
type subst   = Subst1.subst

(* Matching *)
val matchl          : subst -> (term * term) list -> subst
val match           : term -> term -> subst
val matchl_literals : subst -> (formula * formula) list -> subst
val match_literals  : formula -> formula -> subst

(* Unification *)
val unifyl          : subst -> (term * term) list -> subst
val unify           : subst -> term -> term -> subst
val unify_and_apply : term -> term -> term
val unifyl_literals : subst -> (formula * formula) list -> subst
val unify_literals  : subst -> formula -> formula -> subst

end
