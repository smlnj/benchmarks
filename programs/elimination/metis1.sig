(*#line 0.0 "src/Metis1.sig"*)
(* ========================================================================= *)
(* THE METIS COMBINATION OF PROOF PROCEDURES FOR FIRST-ORDER LOGIC           *)
(* Created by Joe Hurd, September 2001                                       *)
(* ========================================================================= *)

signature Metis1 =
sig

type formula     = Term1.formula
type thm         = Thm1.thm
type limit       = Meter1.limit
type solver      = Solver1.solver
type solver_node = Solver1.solver_node

(* Tuning parameters *)
type Mparm = Meson1.parameters
type Rparm = Resolution1.parameters
type parameters =
  {meson           : bool,
   delta           : bool,
   resolution      : bool,
   meson_parm      : Mparm,
   resolution_parm : Rparm}

val defaults                    : parameters
val update_parm_meson           : (bool -> bool) -> parameters -> parameters
val update_parm_delta           : (bool -> bool) -> parameters -> parameters
val update_parm_resolution      : (bool -> bool) -> parameters -> parameters
val update_parm_meson_parm      : (Mparm -> Mparm) -> parameters -> parameters
val update_parm_resolution_parm : (Rparm -> Rparm) -> parameters -> parameters

(* The metis combination of solvers *)
val metis' : parameters -> solver_node
val metis  : solver_node                (* Uses defaults *)

(* A user-friendly interface *)
val settings  : parameters ref          (* Starts off as defaults *)
val limit     : limit ref               (* Starts off as 10 seconds *)
val raw_prove : formula -> thm option   (* Expects a ==> b ==> F *)
val prove     : formula -> thm option   (* Adds eq axioms, converts to CNF *)
val query     : formula -> solver       (* Prolog query engine *)

end
