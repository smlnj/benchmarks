(*#line 0.0 "src/Metis1.sml"*)
(* ========================================================================= *)
(* THE METIS COMBINATION OF PROOF PROCEDURES FOR FIRST-ORDER LOGIC           *)
(* Created by Joe Hurd, September 2001                                       *)
(* ========================================================================= *)

(*
app load
 ["Useful", "Mosml", "Term1", "Thm1", "Canon1",
  "Solver1", "Meson1", "Resolution1"];
*)

(*
*)
structure Metis1 :> Metis1 =
struct

open Useful Term1 Thm1 Meter1 Canon1 Solver1 Meson1 Resolution1;

infix |-> ::> @> oo ## ::* ::@;

(* ------------------------------------------------------------------------- *)
(* Tuning parameters.                                                        *)
(* ------------------------------------------------------------------------- *)

type Mparm = Meson1.parameters;
type Rparm = Resolution1.parameters;

type parameters =
  {meson           : bool,
   delta           : bool,
   resolution      : bool,
   meson_parm      : Mparm,
   resolution_parm : Rparm};

val defaults =
  {meson           = true,
   delta           = true,
   resolution      = true,
   meson_parm      = Meson1.defaults,
   resolution_parm = Resolution1.defaults};

fun update_parm_meson f parm =
  let
    val {meson, delta, resolution, meson_parm, resolution_parm} = parm
  in
    {meson = f meson, delta = delta, resolution = resolution,
     meson_parm = meson_parm, resolution_parm = resolution_parm}
  end;

fun update_parm_delta f parm =
  let
    val {meson, delta, resolution, meson_parm, resolution_parm} = parm
  in
    {meson = meson, delta = f delta, resolution = resolution,
     meson_parm = meson_parm, resolution_parm = resolution_parm}
  end;

fun update_parm_resolution f parm =
  let
    val {meson, delta, resolution, meson_parm, resolution_parm} = parm
  in
    {meson = meson, delta = delta, resolution = f resolution,
     meson_parm = meson_parm, resolution_parm = resolution_parm}
  end;

fun update_parm_meson_parm f parm =
  let
    val {meson, delta, resolution, meson_parm, resolution_parm} = parm
  in
    {meson = meson, delta = delta, resolution = resolution,
     meson_parm = f meson_parm, resolution_parm = resolution_parm}
  end;

fun update_parm_resolution_parm f parm =
  let
    val {meson, delta, resolution, meson_parm, resolution_parm} = parm
  in
    {meson = meson, delta = delta, resolution = resolution,
     meson_parm = meson_parm, resolution_parm = f resolution_parm}
  end;

(* ------------------------------------------------------------------------- *)
(* The metis combination of solvers.                                         *)
(* ------------------------------------------------------------------------- *)

fun metis' {meson = m, delta = d, resolution = r, meson_parm, resolution_parm} =
  combine
  ((if m then cons (time1, meson' meson_parm) else I)
   ((if r then cons (time1, resolution' resolution_parm) else I)
    ((if d then cons (time2, delta' meson_parm) else I)
     [])));

val metis = metis' defaults;

(* ------------------------------------------------------------------------- *)
(* A user-friendly interface.                                                *)
(* ------------------------------------------------------------------------- *)

val settings = ref defaults;

val limit : limit ref = ref {time = NONE, infs = NONE};

fun raw_prove (Imp (a, Imp (b, False))) =
  let
    val (thms, hyps) = (axiomatize a, axiomatize b)
    val solv = metis' (!settings)
  in
    refute (initialize solv {limit = !limit, thms = thms, hyps = hyps})
  end
  | raw_prove _ = raise ERR "raw_prove" "formula not of type a ==> b ==> F";

fun prove g =
  let
    val hyps = eq_axiomatize' (Not (generalize g))
    val solv = set_of_support all_negative (metis' (!settings))
  in
    refute (initialize solv {limit = !limit, thms = [], hyps = hyps})
  end;

fun query database =
  initialize prolog {thms = axiomatize database, hyps = [], limit = unlimited};

(* quick testing
val time = Mosml.time;
quotation := true;
installPP pp_term;
installPP pp_formula;
installPP Subst1.pp_subst;
installPP pp_thm;

(* Testing the metis prover *)

prove True;

val p59 = parse_formula [QUOTE "(!x. P(x) <=> ~P(f(x))) ==> (?x. P(x) /\\ ~P(f(x)))"];
val ths = axiomatize (Not (generalize p59));
prove p59;

val p39 = parse_formula [QUOTE "~(?x. !y. P(y,x) <=> ~P(y,y))"];
clausal (Not (generalize p39));
axiomatize (Not (generalize p39));
prove p39;

val num14 = parse_formula
  [
QUOTE "(!X. product(X, X, square(X))) /\\\n   (!Z X Y. ~product(X, Y, Z) \\/ product(Y, X, Z)) /\\\n   (!Z X Y. ~product(X, Y, Z) \\/ divides(X, Z)) /\\\n   (!Y X V Z.\n      ~prime(X) \\/ ~product(Y, Z, V) \\/ ~divides(X, V) \\/ divides(X, Y) \\/\n      divides(X, Z)) /\\ prime(a) /\\\n   product(a, square(c), square(b)) /\\ ~divides(a, b) ==> F"];
prove num14;

val p26 = parse_formula
 [
QUOTE "((?x. P(x)) <=> (?x. Q(x))) /\\\n   (!x y. P(x) /\\ Q(y) ==> (R(x) <=> U(y))) ==>\n   ((!x. P(x) ==> R(x)) <=> (!x. Q(x) ==> U(x)))"];
clausal (Not (generalize p26));
prove p26;

val los = parse_formula
 [
QUOTE "(!x y z. P x y ==> P y z ==> P x z) /\\\n  (!x y z. Q x y ==> Q y z ==> Q x z) /\\ (!x y. Q x y ==> Q y x) /\\\n  (!x y. P x y \\/ Q x y) ==> (!x y. P x y) \\/ !x y. Q x y"];
try prove los;

val puz2 = parse_formula
  [
QUOTE "(!X. equal(X, X)) /\\ (!Y X. ~equal(X, Y) \\/ equal(Y, X)) /\\\n   (!Z X Y. ~equal(X, Y) \\/ ~equal(Y, Z) \\/ equal(X, Z)) /\\\n   (!B A. ~equal(A, B) \\/ equal(every_one_but(A), every_one_but(B))) /\\\n   (!E C D. ~equal(C, D) \\/ ~hates(C, E) \\/ hates(D, E)) /\\\n   (!H F_avoid G.\n      ~equal(F_avoid, G) \\/ ~hates(H, F_avoid) \\/ hates(H, G)) /\\\n   (!K I J. ~equal(I, J) \\/ ~killed(I, K) \\/ killed(J, K)) /\\\n   (!N L M. ~equal(L, M) \\/ ~killed(N, L) \\/ killed(N, M)) /\\\n   (!P O.\n      ~equal(O, P) \\/ ~lives_at_dreadsbury(O) \\/ lives_at_dreadsbury(P)) /\\\n   (!S Q R. ~equal(Q, R) \\/ ~richer(Q, S) \\/ richer(R, S)) /\\\n   (!V T_avoid U.\n      ~equal(T_avoid, U) \\/ ~richer(V, T_avoid) \\/ richer(V, U)) /\\\n   lives_at_dreadsbury(someone()) /\\ killed(someone(), aunt_agatha()) /\\\n   lives_at_dreadsbury(aunt_agatha()) /\\ lives_at_dreadsbury(butler()) /\\\n   lives_at_dreadsbury(charles()) /\\\n   (!Person.\n      ~lives_at_dreadsbury(Person) \\/ equal(Person, aunt_agatha()) \\/\n      equal(Person, butler()) \\/ equal(Person, charles())) /\\\n   (!Victim Killer. ~killed(Killer, Victim) \\/ hates(Killer, Victim)) /\\\n   (!Victim Killer. ~killed(Killer, Victim) \\/ ~richer(Killer, Victim)) /\\\n   (!Person. ~hates(aunt_agatha(), Person) \\/ ~hates(charles(), Person)) /\\\n   (!Person. equal(Person, butler()) \\/ hates(aunt_agatha(), Person)) /\\\n   (!Person. richer(Person, aunt_agatha()) \\/ hates(butler(), Person)) /\\\n   (!Person. ~hates(aunt_agatha(), Person) \\/ hates(butler(), Person)) /\\\n   (!Person. ~hates(Person, every_one_but(Person))) /\\\n   ~equal(aunt_agatha(), butler()) /\\\n   ~killed(aunt_agatha(), aunt_agatha()) ==> F"];
prove puz2;

val num284 = parse_formula
  [
QUOTE "fibonacci(0, successor(0)) /\\ fibonacci(successor(0), successor(0)) /\\\n   (!N2 N1 N F1 FN F2.\n      ~sum(N1, successor(0), N) \\/ ~sum(N2, successor(successor(0)), N) \\/\n      ~fibonacci(N1, F1) \\/ ~fibonacci(N2, F2) \\/ ~sum(F1, F2, FN) \\/\n      fibonacci(N, FN)) /\\ (!X. sum(X, 0, X)) /\\\n   (!Z X Y. ~sum(X, Y, Z) \\/ sum(X, successor(Y), successor(Z))) /\\\n   (!Result.\n      ~fibonacci(successor(successor(successor(successor(successor(successor(successor(successor(0)))))))),\n                 Result)) ==> F"];
prove num284;

val p29 = parse_formula
  [

QUOTE "(?x. P(x)) /\\ (?x. G(x)) ==>\n   ((!x. P(x) ==> H(x)) /\\ (!x. G(x) ==> J(x)) <=>\n    (!x y. P(x) /\\ G(y) ==> H(x) /\\ J(y)))"];
clausal (Not (generalize p29));
prove p29;

val num27 = parse_formula
  [
QUOTE "(!A. equalish(add(A, 0), A)) /\\\n   (!A B. equalish(add(A, successor(B)), successor(add(A, B)))) /\\\n   (!A. equalish(multiply(A, 0), 0)) /\\\n   (!A B. equalish(multiply(A, successor(B)), add(multiply(A, B), A))) /\\\n   (!B A. ~equalish(successor(A), successor(B)) \\/ equalish(A, B)) /\\\n   (!B A. ~equalish(A, B) \\/ equalish(successor(A), successor(B))) /\\\n   (!C A B. ~less(A, B) \\/ ~less(C, A) \\/ less(C, B)) /\\\n   (!C B A. ~equalish(add(successor(A), B), C) \\/ less(B, C)) /\\\n   (!B A.\n      ~less(A, B) \\/\n      equalish(add(successor(predecessor_of_1st_minus_2nd(B, A)), A),\n        B)) /\\ (!X. equalish(X, X)) /\\\n   (!Y X. ~equalish(X, Y) \\/ equalish(Y, X)) /\\\n   (!Z X Y. ~equalish(X, Y) \\/ ~equalish(Y, Z) \\/ equalish(X, Z)) /\\\n   (!C A B. ~equalish(A, B) \\/ equalish(multiply(A, C), multiply(B, C))) /\\\n   (!B A. ~less(A, B) \\/ ~equalish(A, B)) /\\\n   (!B A. less(A, B) \\/ equalish(B, A) \\/ less(B, A)) /\\\n   (!A. ~less(A, A)) /\\ (!A. ~equalish(successor(A), 0)) /\\\n   (!C A B.\n      ~less(A, B) \\/ equalish(C, 0) \\/\n      less(multiply(A, C), multiply(B, C))) /\\ ~less(b(), a()) /\\\n   less(multiply(b(), c()), multiply(a(), c())) /\\ ~equalish(c(), 0) ==>\n   F"];
prove num27;

val model_completeness = parse_formula
  [
QUOTE "(!M p. sentence(p) ==> holds(M,p) \\/ holds(M,not(p))) /\\\n   (!M p. ~(holds(M,p) /\\ holds(M,not(p)))) ==>\n   ((!p.\n       sentence(p) ==>\n       (!M. models(M,S) ==> holds(M,p)) \\/\n       (!M. models(M,S) ==> holds(M,not(p)))) <=>\n    (!M M'.\n       models(M,S) /\\ models(M',S) ==>\n       (!p. sentence(p) ==> (holds(M,p) <=> holds(M',p)))))"];
prove model_completeness;

val agatha = parse_formula
  [
QUOTE "lives(agatha()) /\\ lives(butler()) /\\ lives(charles()) /\\\n   (killed(agatha(),agatha()) \\/ killed(butler(),agatha()) \\/\n    killed(charles(),agatha())) /\\\n   (!x y. killed(x,y) ==> hates(x,y) /\\ ~richer(x,y)) /\\\n   (!x. hates(agatha(),x) ==> ~hates(charles(),x)) /\\\n   (hates(agatha(),agatha()) /\\ hates(agatha(),charles())) /\\\n   (!x. lives(x) /\\ ~richer(x,agatha()) ==> hates(butler(),x)) /\\\n   (!x. hates(agatha(),x) ==> hates(butler(),x)) /\\\n   (!x. ~hates(x,agatha()) \\/ ~hates(x,butler()) \\/ ~hates(x,charles()))\n   ==>\n   killed(agatha(),agatha()) /\\ ~killed(butler(),agatha()) /\\\n   ~killed(charles(),agatha())"];
prove agatha;

val boo3 = parse_formula
 [
QUOTE "(!X. equal(X, X)) /\\ (!Y X. ~equal(X, Y) \\/ equal(Y, X)) /\\\n  (!Z X Y. ~equal(X, Y) \\/ ~equal(Y, Z) \\/ equal(X, Z)) /\\\n  (!Y X. sum(X, Y, add(X, Y))) /\\ (!Y X. product(X, Y, multiply(X, Y))) /\\\n  (!Z X Y. ~sum(X, Y, Z) \\/ sum(Y, X, Z)) /\\\n  (!Z X Y. ~product(X, Y, Z) \\/ product(Y, X, Z)) /\\\n  (!X. sum(additive_identity(), X, X)) /\\\n  (!X. sum(X, additive_identity(), X)) /\\\n  (!X. product(multiplicative_identity(), X, X)) /\\\n  (!X. product(X, multiplicative_identity(), X)) /\\\n  (!Z X Y V1 V3 V4 V2.\n     ~product(X, Y, V1) \\/ ~product(X, Z, V2) \\/ ~sum(Y, Z, V3) \\/\n     ~product(X, V3, V4) \\/ sum(V1, V2, V4)) /\\\n  (!Z X Y V1 V3 V4 V2.\n     ~product(X, Y, V1) \\/ ~product(X, Z, V2) \\/ ~sum(Y, Z, V3) \\/\n     ~sum(V1, V2, V4) \\/ product(X, V3, V4)) /\\\n  (!Z Y X V1 V3 V4 V2.\n     ~product(Y, X, V1) \\/ ~product(Z, X, V2) \\/ ~sum(Y, Z, V3) \\/\n     ~product(V3, X, V4) \\/ sum(V1, V2, V4)) /\\\n  (!Z Y X V1 V3 V4 V2.\n     ~product(Y, X, V1) \\/ ~product(Z, X, V2) \\/ ~sum(Y, Z, V3) \\/\n     ~sum(V1, V2, V4) \\/ product(V3, X, V4)) /\\\n  (!Z X Y V1 V3 V4 V2.\n     ~sum(X, Y, V1) \\/ ~sum(X, Z, V2) \\/ ~product(Y, Z, V3) \\/\n     ~sum(X, V3, V4) \\/ product(V1, V2, V4)) /\\\n  (!Z X Y V1 V3 V4 V2.\n     ~sum(X, Y, V1) \\/ ~sum(X, Z, V2) \\/ ~product(Y, Z, V3) \\/\n     ~product(V1, V2, V4) \\/ sum(X, V3, V4)) /\\\n  (!Z Y X V1 V3 V4 V2.\n     ~sum(Y, X, V1) \\/ ~sum(Z, X, V2) \\/ ~product(Y, Z, V3) \\/\n     ~sum(V3, X, V4) \\/ product(V1, V2, V4)) /\\\n  (!Z Y X V1 V3 V4 V2.\n     ~sum(Y, X, V1) \\/ ~sum(Z, X, V2) \\/ ~product(Y, Z, V3) \\/\n     ~product(V1, V2, V4) \\/ sum(V3, X, V4)) /\\\n  (!X. sum(inverse(X), X, multiplicative_identity())) /\\\n  (!X. sum(X, inverse(X), multiplicative_identity())) /\\\n  (!X. product(inverse(X), X, additive_identity())) /\\\n  (!X. product(X, inverse(X), additive_identity())) /\\\n  (!V X Y U. ~sum(X, Y, U) \\/ ~sum(X, Y, V) \\/ equal(U, V)) /\\\n  (!V X Y U. ~product(X, Y, U) \\/ ~product(X, Y, V) \\/ equal(U, V)) /\\\n  (!W X Y Z. ~equal(X, Y) \\/ ~sum(X, W, Z) \\/ sum(Y, W, Z)) /\\\n  (!W X Y Z. ~equal(X, Y) \\/ ~sum(W, X, Z) \\/ sum(W, Y, Z)) /\\\n  (!W X Y Z. ~equal(X, Y) \\/ ~sum(W, Z, X) \\/ sum(W, Z, Y)) /\\\n  (!W X Y Z. ~equal(X, Y) \\/ ~product(X, W, Z) \\/ product(Y, W, Z)) /\\\n  (!W X Y Z. ~equal(X, Y) \\/ ~product(W, X, Z) \\/ product(W, Y, Z)) /\\\n  (!W X Y Z. ~equal(X, Y) \\/ ~product(W, Z, X) \\/ product(W, Z, Y)) /\\\n  (!W X Y. ~equal(X, Y) \\/ equal(add(X, W), add(Y, W))) /\\\n  (!W X Y. ~equal(X, Y) \\/ equal(add(W, X), add(W, Y))) /\\\n  (!W X Y. ~equal(X, Y) \\/ equal(multiply(X, W), multiply(Y, W))) /\\\n  (!W X Y. ~equal(X, Y) \\/ equal(multiply(W, X), multiply(W, Y))) /\\\n  (!Y X. ~equal(X, Y) \\/ equal(inverse(X), inverse(Y))) /\\\n  ~product(x(), x(), x()) ==> F"];
prove boo3;

val fld5 = parse_formula
 [
QUOTE "(!Y X V W Z U.\n     sum(X, V, W) \\/ ~sum(X, Y, U) \\/ ~sum(Y, Z, V) \\/ ~sum(U, Z, W)) /\\\n  (!X U Z W V Y.\n     sum(U, Z, W) \\/ ~sum(X, Y, U) \\/ ~sum(Y, Z, V) \\/ ~sum(X, V, W)) /\\\n  (!X. sum(additive_identity(), X, X) \\/ ~defined(X)) /\\\n  (!X. sum(additive_inverse(X), X, additive_identity()) \\/ ~defined(X)) /\\\n  (!Z Y X. sum(Y, X, Z) \\/ ~sum(X, Y, Z)) /\\\n  (!Y X V W Z U.\n     product(X, V, W) \\/ ~product(X, Y, U) \\/ ~product(Y, Z, V) \\/\n     ~product(U, Z, W)) /\\\n  (!X U Z W V Y.\n     product(U, Z, W) \\/ ~product(X, Y, U) \\/ ~product(Y, Z, V) \\/\n     ~product(X, V, W)) /\\\n  (!X. product(multiplicative_identity(), X, X) \\/ ~defined(X)) /\\\n  (!X.\n     product(multiplicative_inverse(X), X, multiplicative_identity()) \\/\n     sum(additive_identity(), X, additive_identity()) \\/ ~defined(X)) /\\\n  (!Z Y X. product(Y, X, Z) \\/ ~product(X, Y, Z)) /\\\n  (!X C D B Z A Y.\n     sum(C, D, B) \\/ ~sum(X, Y, A) \\/ ~product(A, Z, B) \\/\n     ~product(X, Z, C) \\/ ~product(Y, Z, D)) /\\\n  (!X A Z B C D Y.\n     product(A, Z, B) \\/ ~sum(X, Y, A) \\/ ~product(X, Z, C) \\/\n     ~product(Y, Z, D) \\/ ~sum(C, D, B)) /\\\n  (!X Y. defined(add(X, Y)) \\/ ~defined(X) \\/ ~defined(Y)) /\\\n  defined(additive_identity()) /\\\n  (!X. defined(additive_inverse(X)) \\/ ~defined(X)) /\\\n  (!X Y. defined(multiply(X, Y)) \\/ ~defined(X) \\/ ~defined(Y)) /\\\n  defined(multiplicative_identity()) /\\\n  (!X.\n     defined(multiplicative_inverse(X)) \\/ ~defined(X) \\/\n     sum(additive_identity(), X, additive_identity())) /\\\n  (!Y X. sum(X, Y, add(X, Y)) \\/ ~defined(X) \\/ ~defined(Y)) /\\\n  (!Y X. product(X, Y, multiply(X, Y)) \\/ ~defined(X) \\/ ~defined(Y)) /\\\n  (!Y X.\n     sum(additive_identity(), X, Y) \\/ ~less_or_equal(X, Y) \\/\n     ~less_or_equal(Y, X)) /\\\n  (!Y X Z.\n     less_or_equal(X, Z) \\/ ~less_or_equal(X, Y) \\/\n     ~less_or_equal(Y, Z)) /\\\n  (!Y X.\n     less_or_equal(X, Y) \\/ less_or_equal(Y, X) \\/ ~defined(X) \\/\n     ~defined(Y)) /\\\n  (!X U V Z Y.\n     less_or_equal(U, V) \\/ ~less_or_equal(X, Y) \\/ ~sum(X, Z, U) \\/\n     ~sum(Y, Z, V)) /\\\n  (!X Z Y.\n     less_or_equal(additive_identity(), Z) \\/\n     ~less_or_equal(additive_identity(), X) \\/\n     ~less_or_equal(additive_identity(), Y) \\/ ~product(X, Y, Z)) /\\\n  ~sum(additive_identity(), additive_identity(),\n     multiplicative_identity()) /\\ defined(a()) /\\ defined(b()) /\\\n  (!X. ~sum(a(), X, b())) ==> F"];
prove fld5;
*)

end
