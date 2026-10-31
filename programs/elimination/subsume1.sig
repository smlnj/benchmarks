(*#line 0.0 "fol/Subsume1.sig"*)
(* ========================================================================= *)
(* A TYPE FOR SUBSUMPTION CHECKING                                           *)
(* Created by Joe Hurd, April 2002                                           *)
(* ========================================================================= *)

signature Subsume1 =
sig

type 'a pp           = 'a Useful.pp
type ('a, 'b) maplet = ('a, 'b) Useful.maplet
type formula         = Term1.formula
type subst           = Subst1.subst

type 'a subsume

val empty             : 'a subsume
val add               : (formula list, 'a) maplet -> 'a subsume -> 'a subsume
val subsumed          : 'a subsume -> formula list -> (subst * 'a) list
val strictly_subsumed : 'a subsume -> formula list -> (subst * 'a) list
val info              : 'a subsume -> string
val pp_subsum         : 'a subsume pp

end
