(*#line 0.0 "src/Resolvers1.sig"*)
(* ========================================================================= *)
(* A TYPE TO FIND RESOLVANT CLAUSES                                          *)
(* Created by Joe Hurd, April 2002                                           *)
(* ========================================================================= *)

signature Resolvers1 =
sig

type 'a pp   = 'a Useful.pp
type formula = Term1.formula
type subst   = Subst1.subst
type thm     = Thm1.thm

type resolvers
type resolvant = {mate : thm, sub : subst, res : thm}

val empty_resolvers : resolvers
val add_resolver    : thm -> resolvers -> resolvers
val find_resolvants : resolvers -> thm -> resolvant list
val resolvers_info  : resolvers -> string
val pp_resolvers    : resolvers pp

end
