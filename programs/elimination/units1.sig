(*#line 0.0 "fol/Units1.sig"*)
(* ========================================================================= *)
(* A STORE IN WHICH TO CACHE UNIT THEOREMS                                   *)
(* Created by Joe Hurd, November 2001                                        *)
(* ========================================================================= *)

signature Units1 =
sig

type 'a pp   = 'a Useful.pp
type formula = Term1.formula
type thm     = Thm1.thm

type units

val empty    : units
val add      : thm -> units -> units
val addl     : thm list -> units -> units
val subsumes : units -> formula -> thm option
val prove    : units -> formula list -> thm list option
val demod    : units -> thm -> thm
val info     : units -> string
val pp_units : units pp

end
