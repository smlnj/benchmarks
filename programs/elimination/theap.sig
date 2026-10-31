(*#line 0.0 "src/Theap1.sig"*)
(* ========================================================================= *)
(* A TYPE TO STORE CLAUSES WAITING TO BE USED (THEAP = THEOREM HEAP)         *)
(* Created by Joe Hurd, April 2002                                           *)
(* ========================================================================= *)

signature Theap1 =
sig

type 'a subsume = 'a Subsume1.subsume
type thm        = Thm1.thm

(* Tuning parameters *)
type parameters = {fifo_skew : int, cleaning_freq : int}
val defaults : parameters

(* Theorem HEAPs *)
type theap
val new_theap       : parameters -> theap
val empty_theap     : theap                       (* Uses defaults *)
val theap_size      : theap -> int
val theap_add       : thm -> theap -> theap
val theap_addl      : thm list -> theap -> theap
val theap_remove    : theap -> (thm * theap) option
val theap_subsumers : theap -> thm subsume
val theap_info      : theap -> string   (* Outputs "(size,weight)" *)

end
