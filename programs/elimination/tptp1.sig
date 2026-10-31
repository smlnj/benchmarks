(*#line 0.0 "fol/Tptp1.sig"*)
(* ========================================================================= *)
(* INTERFACE TO TPTP PROBLEM FILES                                           *)
(* Created by Joe Hurd, December 2001                                        *)
(* ========================================================================= *)

signature Tptp1 =
sig

type term    = Term1.term
type formula = Term1.formula

(* Maintaining different relation and function names in TPTP problems *)
val renaming : {tptp : string, fol : string, arity : int} list ref

(* Parsing: pass in a filename *)
val parse_cnf : string -> formula

end
