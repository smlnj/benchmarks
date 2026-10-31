(*#line 0.0 "fol/Problem1.sig"*)
(* ========================================================================= *)
(* SOME SAMPLE PROBLEMS TO TEST PROOF PROCEDURES                             *)
(* Created by Joe Hurd, September 2001                                       *)
(* Partly ported from the CAML-Light code accompanying John Harrison's book  *)
(* ========================================================================= *)

signature Problem1 =
sig

type 'a quotation = 'a Parser.frag list
type 'a problem   = {name : string, goal : 'a quotation}

(* Accessing individual problems *)
val get : 'a problem list -> string -> 'a quotation

(* The master collections *)
val nonequality : 'a problem list
val equality    : 'a problem list
val tptp        : 'a problem list

(* Some compilations *)
(*val quick : 'a problem list *)

end
