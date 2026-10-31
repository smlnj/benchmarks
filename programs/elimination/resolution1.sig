(*#line 0.0 "src/Resolution1.sig"*)
(* ========================================================================= *)
(* THE RESOLUTION PROOF PROCEDURE                                            *)
(* Created by Joe Hurd, November 2001                                        *)
(* ========================================================================= *)

signature Resolution1 =
sig

type solver_node = Solver1.solver_node

(* Tuning parameters *)
type parameters =
  {subsumption_checking : int,                    (* in the range 0..3 *)
   positive_refinement  : bool,
   theap_parm           : Theap1.parameters}

val defaults : parameters

(* Resolution *)
val resolution' : parameters -> solver_node
val resolution  : solver_node                     (* Uses defaults *)

end
