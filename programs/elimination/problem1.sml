(*#line 0.0 "fol/Problem1.sml"*)
(* ========================================================================= *)
(* SOME SAMPLE PROBLEMS TO TEST PROOF PROCEDURES                             *)
(* Created by Joe Hurd, September 2001                                       *)
(* Partly ported from the CAML-Light code accompanying John Harrison's book  *)
(* ========================================================================= *)

structure Problem1 :> Problem1 =
struct

datatype frag = datatype Parser.frag

type 'a quotation = 'a frag list;

type 'a problem = {name : string, goal : 'a quotation};

(* ========================================================================= *)
(* Accessing individual problems.                                            *)
(* ========================================================================= *)

fun extract (p : 'a problem list) n =
  Option.valOf (List.find (fn {name, ...} => name = n) p);

fun get p = #goal o extract p;

(* ========================================================================= *)
(* Problems without equality.                                                *)
(* ========================================================================= *)

val nonequality = [

(* ------------------------------------------------------------------------- *)
(* Trivia (some of which demonstrate ex-bugs in provers).                    *)
(* ------------------------------------------------------------------------- *)

{name = "TRUE",
 goal = [
QUOTE "\nT"]},

{name = "P_or_not_P",
 goal = [
QUOTE "\np \\/ ~p"]},

{name = "JH_test",
 goal = [
QUOTE "\n!x y. ?z. p x \\/ p y ==> p z"]},

{name = "CYCLIC",
 goal = [
QUOTE "\n(!x. p (g (c x))) ==> ?z. p (g z)"]},

{name = "MN_bug",
 goal = [
QUOTE "\n(!x. ?y. f y x x) ==> ?z. f z 0 0"]},

{name = "ERIC",
 goal = [
QUOTE "\n!x. ?v w. !y z. p x /\\ q y ==> (p v \\/ r w) /\\ (r z ==> q v)"]},

(* ------------------------------------------------------------------------- *)
(* Propositional Logic.                                                      *)
(* ------------------------------------------------------------------------- *)

{name = "PROP_1",
 goal = [
QUOTE "\np ==> q <=> ~q ==> ~p"]},

{name = "PROP_2",
 goal = [
QUOTE "\n~~p <=> p"]},

{name = "PROP_3",
 goal = [
QUOTE "\n~(p ==> q) ==> q ==> p"]},

{name = "PROP_4",
 goal = [
QUOTE "\n~p ==> q <=> ~q ==> p"]},

{name = "PROP_5",
 goal = [
QUOTE "\n(p \\/ q ==> p \\/ r) ==> p \\/ (q ==> r)"]},

{name = "PROP_6",
 goal = [
QUOTE "\np \\/ ~p"]},

{name = "PROP_7",
 goal = [
QUOTE "\np \\/ ~~~p"]},

{name = "PROP_8",
 goal = [
QUOTE "\n((p ==> q) ==> p) ==> p"]},

{name = "PROP_9",
 goal = [
QUOTE "\n(p \\/ q) /\\ (~p \\/ q) /\\ (p \\/ ~q) ==> ~(~q \\/ ~q)"]},

{name = "PROP_10",
 goal = [
QUOTE "\n(q ==> r) /\\ (r ==> p /\\ q) /\\ (p ==> q /\\ r) ==> (p <=> q)"]},

{name = "PROP_11",
 goal = [
QUOTE "\np <=> p"]},

{name = "PROP_12",
 goal = [
QUOTE "\n((p <=> q) <=> r) <=> p <=> q <=> r"]},

{name = "PROP_13",
 goal = [
QUOTE "\np \\/ q /\\ r <=> (p \\/ q) /\\ (p \\/ r)"]},

{name = "PROP_14",
 goal = [
QUOTE "\n(p <=> q) <=> (q \\/ ~p) /\\ (~q \\/ p)"]},

{name = "PROP_15",
 goal = [
QUOTE "\np ==> q <=> ~p \\/ q"]},

{name = "PROP_16",
 goal = [
QUOTE "\n(p ==> q) \\/ (q ==> p)"]},

{name = "PROP_17",
 goal = [
QUOTE "\np /\\ (q ==> r) ==> s <=> (~p \\/ q \\/ s) /\\ (~p \\/ ~r \\/ s)"]},

{name = "MATHS4_EXAMPLE",
 goal = [
QUOTE "\n(a \\/ ~k ==> g) /\\ (g ==> q) /\\ ~q ==> k"]},

{name = "XOR_ASSOC",
 goal = [
QUOTE "\n~(~(p <=> q) <=> r) <=> ~(p <=> ~(q <=> r))"]},

(* ------------------------------------------------------------------------- *)
(* Monadic Predicate Logic.                                                  *)
(* ------------------------------------------------------------------------- *)

(* The drinker's principle *)
{name = "P18",
 goal = [
QUOTE "\n?very_popular_guy. !whole_pub. drinks very_popular_guy ==> drinks whole_pub"]},

{name = "P19",
 goal = [
QUOTE "\n?x. !y z. (p y ==> q z) ==> p x ==> q x"]},

{name = "P20",
 goal = [
QUOTE "\n(!x y. ?z. !w. p x /\\ q y ==> r z /\\ u w) /\\ (!x y. p x /\\ q y) ==> ?z. r z"]},

{name = "P21",
 goal = [
QUOTE "\n(?x. p ==> q x) /\\ (?x. q x ==> p) ==> ?x. p <=> q x"]},

{name = "P22",
 goal = [
QUOTE "\n(!x. p <=> q x) ==> (p <=> !x. q x)"]},

{name = "P23",
 goal = [
QUOTE "\n(!x. p \\/ q x) <=> p \\/ !x. q x"]},

{name = "P24",
 goal = [

QUOTE "\n~(?x. u x /\\ q x) /\\ (!x. p x ==> q x \\/ r x) /\\ ~(?x. p x ==> ?x. q x) /\\\n(!x. q x /\\ r x ==> u x) ==> ?x. p x /\\ r x"]},

{name = "P25",
 goal = [

QUOTE "\n(?x. p x) /\\ (!x. u x ==> ~g x /\\ r x) /\\ (!x. p x ==> g x /\\ u x) /\\\n((!x. p x ==> q x) \\/ ?x. q x /\\ p x) ==> ?x. q x /\\ p x"]},

{name = "P26",
 goal = [

QUOTE "\n((?x. p x) <=> ?x. q x) /\\ (!x y. p x /\\ q y ==> (r x <=> u y)) ==>\n((!x. p x ==> r x) <=> !x. q x ==> u x)"]},

{name = "P27",
 goal = [

QUOTE "\n(?x. p x /\\ ~q x) /\\ (!x. p x ==> r x) /\\ (!x. u x /\\ s x ==> p x) /\\\n(?x. r x /\\ ~q x) ==> (!x. u x ==> ~r x) ==> !x. u x ==> ~s x"]},

{name = "P28",
 goal = [

QUOTE "\n(!x. p x ==> !x. q x) /\\ ((!x. q x \\/ r x) ==> ?x. q x /\\ r x) /\\\n((?x. r x) ==> !x. l x ==> m x) ==> !x. p x /\\ l x ==> m x"]},

{name = "P29",
 goal = [


QUOTE "\n(?x. p x) /\\ (?x. g x) ==>\n((!x. p x ==> h x) /\\ (!x. g x ==> j x) <=>\n !x y. p x /\\ g y ==> h x /\\ j y)"]},

{name = "P30",
 goal = [

QUOTE "\n(!x. p x \\/ g x ==> ~h x) /\\ (!x. (g x ==> ~u x) ==> p x /\\ h x) ==>\n!x. u x"]},

{name = "P31",
 goal = [

QUOTE "\n~(?x. p x /\\ (g x \\/ h x)) /\\ (?x. q x /\\ p x) /\\ (!x. ~h x ==> j x) ==>\n?x. q x /\\ j x"]},

{name = "P32",
 goal = [

QUOTE "\n(!x. p x /\\ (g x \\/ h x) ==> q x) /\\ (!x. q x /\\ h x ==> j x) /\\\n(!x. r x ==> h x) ==> !x. p x /\\ r x ==> j x"]},

{name = "P33",
 goal = [

QUOTE "\n(!x. p a /\\ (p x ==> p b) ==> p c) <=>\n(!x. p a ==> p x \\/ p c) /\\ (p a ==> p b ==> p c)"]},

(* This gives rise to 5184 clauses when converted to CNF! *)
{name = "P34",
 goal = [

QUOTE "\n((?x. !y. p x <=> p y) <=> (?x. q x) <=> !y. q y) <=>\n(?x. !y. q x <=> q y) <=> (?x. p x) <=> !y. p y"]},

{name = "P35",
 goal = [
QUOTE "\n?x y. p x y ==> !x y. p x y"]},

(* ------------------------------------------------------------------------- *)
(* Full predicate logic (without Identity and Functions)                     *)
(* ------------------------------------------------------------------------- *)

{name = "P36",
 goal = [

QUOTE "\n(!x. ?y. p x y) /\\ (!x. ?y. g x y) /\\\n(!x y. p x y \\/ g x y ==> !z. p y z \\/ g y z ==> h x z) ==> !x. ?y. h x y"]},

{name = "P37",
 goal = [


QUOTE "\n(!z. ?w. !x. ?y. (p x z ==> p y w) /\\ p y z /\\ (p y w ==> ?v. q v w)) /\\\n(!x z. ~p x z ==> ?y. q y z) /\\ ((?x y. q x y) ==> !x. r x x) ==>\n!x. ?y. r x y"]},

{name = "P38",
 goal = [



QUOTE "\n(!x. p a /\\ (p x ==> ?y. p y /\\ r x y) ==> ?z w. p z /\\ r x w /\\ r w z) <=>\n!x.\n  (~p a \\/ p x \\/ ?z w. p z /\\ r x w /\\ r w z) /\\\n  (~p a \\/ ~(?y. p y /\\ r x y) \\/ ?z w. p z /\\ r x w /\\ r w z)"]},

{name = "P39",
 goal = [
QUOTE "\n~?x. !y. p y x <=> ~p y y"]},

{name = "P40",
 goal = [
QUOTE "\n(?y. !x. p x y <=> p x x) ==> ~!x. ?y. !z. p z y <=> ~p z x"]},

{name = "P41",
 goal = [
QUOTE "\n(!z. ?y. !x. p x y <=> p x z /\\ ~p x x) ==> ~?z. !x. p x z"]},

{name = "P42",
 goal = [
QUOTE "\n~?y. !x. p x y <=> ~?z. p x z /\\ p z x"]},

{name = "P43",
 goal = [
QUOTE "\n(!x y. q x y <=> !z. p z x <=> p z y) ==> !x y. q x y <=> q y x"]},

{name = "P44",
 goal = [

QUOTE "\n(!x. p x ==> (?y. g y /\\ h x y) /\\ ?y. g y /\\ ~h x y) /\\\n(?x. j x /\\ !y. g y ==> h x y) ==> ?x. j x /\\ ~p x"]},

{name = "P45",
 goal = [



QUOTE "\n(!x. p x /\\ (!y. g y /\\ h x y ==> j x y) ==> !y. g y /\\ h x y ==> r y) /\\\n~(?y. l y /\\ r y) /\\\n(?x. p x /\\ (!y. h x y ==> l y) /\\ !y. g y /\\ h x y ==> j x y) ==>\n?x. p x /\\ ~?y. g y /\\ h x y"]},

{name = "P46",
 goal = [


QUOTE "\n(!x. p x /\\ (!y. p y /\\ h y x ==> g y) ==> g x) /\\\n((?x. p x /\\ ~g x) ==> ?x. p x /\\ ~g x /\\ !y. p y /\\ ~g y ==> j x y) /\\\n(!x y. p x /\\ p y /\\ h x y ==> ~j y x) ==> !x. p x ==> g x"]},

{name = "P50",
 goal = [
QUOTE "\n(!x. f0 a x \\/ !y. f0 x y) ==> ?x. !y. f0 x y"]},

(* ------------------------------------------------------------------------- *)
(* Example from Manthey and Bry, CADE-9.                                     *)
(* ------------------------------------------------------------------------- *)

{name = "P55",
 goal = [








QUOTE "\nlives agatha /\\ lives butler /\\ lives charles /\\\n(killed agatha agatha \\/ killed butler agatha \\/ killed charles agatha) /\\\n(!x y. killed x y ==> hates x y /\\ ~richer x y) /\\\n(!x. hates agatha x ==> ~hates charles x) /\\\n(hates agatha agatha /\\ hates agatha charles) /\\\n(!x. lives x /\\ ~richer x agatha ==> hates butler x) /\\\n(!x. hates agatha x ==> hates butler x) /\\\n(!x. ~hates x agatha \\/ ~hates x butler \\/ ~hates x charles) ==>\nkilled agatha agatha /\\ ~killed butler agatha /\\ ~killed charles agatha"]},

{name = "P57",
 goal = [

QUOTE "\np (f a b) (f b c) /\\ p (f b c) (f a c) /\\\n(!x y z. p x y /\\ p y z ==> p x z) ==> p (f a b) (f a c)"]},

(* ------------------------------------------------------------------------- *)
(* See info-hol, circa 1500.                                                 *)
(* ------------------------------------------------------------------------- *)

{name = "P58",
 goal = [
QUOTE "\n!x. ?v w. !y z. p x /\\ q y ==> (p v \\/ r w) /\\ (r z ==> q v)"]},

{name = "P59",
 goal = [
QUOTE "\n(!x. p x <=> ~p (f x)) ==> ?x. p x /\\ ~p (f x)"]},

{name = "P60",
 goal = [
QUOTE "\n!x. p x (f x) <=> ?y. (!z. p z y ==> p z (f x)) /\\ p x y"]},

(* ------------------------------------------------------------------------- *)
(* From Gilmore's classic paper.                                             *)
(* ------------------------------------------------------------------------- *)

(*
JRH: Amazingly, this still seems non-trivial... in HOL it works at depth 45!
Joe: Confirmed (depth=45, inferences=263702, time=148s), though if lemmaizing
     is used then a lemma is discovered at depth 29 that allows a much quicker
     proof (depth=30, inferences=10039, time=5.5s). [13 Oct 2001]
*)
{name = "GILMORE_1",
 goal = [


QUOTE "\n?x. !y z.\n  (f y ==> g y <=> f x) /\\ (f y ==> h y <=> g x) /\\\n  ((f y ==> g y) ==> h y <=> h x) ==> f z /\\ g z /\\ h z"]},

(*
JRH: This is not valid, according to Gilmore
{name = "GILMORE_2",
 goal = `
?x y. !z.
  (f x z <=> f z y) /\ (f z y <=> f z z) /\ (f x y <=> f y x) ==>
  (f x y <=> f x z)`},
*)

{name = "GILMORE_3",
 goal = [


QUOTE "\n?x. !y z.\n  ((f y z ==> g y ==> h x) ==> f x x) /\\ ((f z x ==> g x) ==> h z) /\\\n  f x y ==> f z z"]},

{name = "GILMORE_4",
 goal = [
QUOTE "\n?x y. !z. (f x y ==> f y z /\\ f z z) /\\ (f x y /\\ g x y ==> g x z /\\ g z z)"]},

{name = "GILMORE_5",
 goal = [
QUOTE "\n(!x. ?y. f x y \\/ f y x) /\\ (!x y. f y x ==> f y y) ==> ?z. f z z"]},

{name = "GILMORE_6",
 goal = [



QUOTE "\n!x. ?y.\n  (?w. !v. f w x ==> g v w /\\ g w x) ==>\n  (?w. !v. f w y ==> g v w /\\ g w y) \\/\n  !z v. ?w. g v z \\/ h w y z ==> g z w"]},

{name = "GILMORE_7",
 goal = [

QUOTE "\n(!x. k x ==> ?y. l y /\\ (f x y ==> g x y)) /\\\n(?z. k z /\\ !w. l w ==> f z w) ==> ?v w. k v /\\ l w /\\ g v w"]},

{name = "GILMORE_8",
 goal = [


QUOTE "\n?x. !y z.\n  ((f y z ==> g y ==> !w. ?v. h w v x) ==> f x x) /\\\n  ((f z x ==> g x) ==> !w. ?v. h w v z) /\\ f x y ==> f z z"]},

(*
JRH: This is still a very hard goal
Joe: With lemmaizing (in HOL): (depth=18, inferences=15632, time=21s)
     Without: gave up waiting after (depth=25, inferences=2125072, time=3000s)
     [13 Oct 2001]
*)
{name = "GILMORE_9",
 goal = [







QUOTE "\n!x. ?y. !z.\n  ((!w. ?v. f y w v /\\ g y w /\\ ~h y x) ==>\n   (!w. ?v. f x w v /\\ g z u /\\ ~h x z) ==>\n   !w. ?v. f x w v /\\ g y w /\\ ~h x y) /\\\n  ((!w. ?v. f x w v /\\ g y w /\\ ~h x y) ==>\n   ~(!w. ?v. f x w v /\\ g z w /\\ ~h x z) ==>\n   (!w. ?v. f y w v /\\ g y w /\\ ~h y x) /\\\n   !w. ?v. f z w v /\\ g y w /\\ ~h z y)"]},

(* ------------------------------------------------------------------------- *)
(* Translation of Gilmore procedure using separate definitions.              *)
(* ------------------------------------------------------------------------- *)

{name = "GILMORE_9a",
 goal = [


QUOTE "\n(!x y. p x y <=> !w. ?v. f x w v /\\ g y w /\\ ~h x y) ==>\n!x. ?y. !z.\n  (p y x ==> p x z ==> p x y) /\\ (p x y ==> ~p x z ==> p y x /\\ p z y)"]},

(* ------------------------------------------------------------------------- *)
(* Example from Davis-Putnam papers where Gilmore procedure is poor.         *)
(* ------------------------------------------------------------------------- *)

{name = "DAVIS_PUTNAM_EXAMPLE",
 goal = [
QUOTE "\n?x y. !z. (f x y ==> f y z /\\ f z z) /\\ (f x y /\\ g x y ==> g x z /\\ g z z)"]},

(* ------------------------------------------------------------------------- *)
(* The interesting example where connections make the proof longer.          *)
(* ------------------------------------------------------------------------- *)

{name = "BAD_CONNECTIONS",
 goal = [

QUOTE "\n~a /\\ (a \\/ b) /\\ (c \\/ d) /\\ (~b \\/ e \\/ f) /\\ (~c \\/ ~e) /\\ (~c \\/ ~f) /\\\n(~b \\/ g \\/ h) /\\ (~d \\/ ~g) /\\ (~d \\/ ~h) ==> F"]},

(* ------------------------------------------------------------------------- *)
(* The classic Los puzzle. (Clausal version MSC006-1 in the TPTP library.)   *)
(* Note: this is actually in the decidable "AE" subset,  though that doesn't *)
(* yield a very efficient proof.                                             *)
(* ------------------------------------------------------------------------- *)

{name = "LOS",
 goal = [


QUOTE "\n(!x y z. p x y ==> p y z ==> p x z) /\\\n(!x y z. q x y ==> q y z ==> q x z) /\\ (!x y. q x y ==> q y x) /\\\n(!x y. p x y \\/ q x y) ==> (!x y. p x y) \\/ !x y. q x y"]},

(* ------------------------------------------------------------------------- *)
(* The steamroller.                                                          *)
(* ------------------------------------------------------------------------- *)

{name = "STEAM_ROLLER",
 goal = [












QUOTE "\n((!x. p1 x ==> p0 x) /\\ ?x. p1 x) /\\ ((!x. p2 x ==> p0 x) /\\ ?x. p2 x) /\\\n((!x. p3 x ==> p0 x) /\\ ?x. p3 x) /\\ ((!x. p4 x ==> p0 x) /\\ ?x. p4 x) /\\\n((!x. p5 x ==> p0 x) /\\ ?x. p5 x) /\\ ((?x. q1 x) /\\ !x. q1 x ==> q0 x) /\\\n(!x.\n   p0 x ==>\n   (!y. q0 y ==> r x y) \\/\n   !y. p0 y /\\ s0 y x /\\ (?z. q0 z /\\ r y z) ==> r x y) /\\\n(!x y. p3 y /\\ (p5 x \\/ p4 x) ==> s0 x y) /\\\n(!x y. p3 x /\\ p2 y ==> s0 x y) /\\ (!x y. p2 x /\\ p1 y ==> s0 x y) /\\\n(!x y. p1 x /\\ (p2 y \\/ q1 y) ==> ~r x y) /\\\n(!x y. p3 x /\\ p4 y ==> r x y) /\\ (!x y. p3 x /\\ p5 y ==> ~r x y) /\\\n(!x. p4 x \\/ p5 x ==> ?y. q0 y /\\ r x y) ==>\n?x y. p0 x /\\ p0 y /\\ ?z. q1 z /\\ r y z /\\ r x y"]},

(* ------------------------------------------------------------------------- *)
(* An incestuous example used to establish completeness characterization.    *)
(* ------------------------------------------------------------------------- *)

{name = "MODEL_COMPLETENESS",
 goal = [








QUOTE "\n(!w x. sentence x ==> holds w x \\/ holds w (not x)) /\\\n(!w x. ~(holds w x /\\ holds w (not x))) ==>\n((!x.\n    sentence x ==>\n    (!w. models w s ==> holds w x) \\/\n    !w. models w s ==> holds w (not x)) <=>\n !w v.\n   models w s /\\ models v s ==>\n   !x. sentence x ==> (holds w x <=> holds v x))"]}

];

(* ========================================================================= *)
(* Problems with equality.                                                   *)
(* ========================================================================= *)

val equality = [

(* ------------------------------------------------------------------------- *)
(* Trivia (some of which demonstrate ex-bugs in the prover).                 *)
(* ------------------------------------------------------------------------- *)

{name = "REFLEXIVITY",
 goal = [
QUOTE "\nc = c"]},

{name = "SYMMETRY",
 goal = [
QUOTE "\n!x y. x = y ==> y = x"]},

{name = "TRANSITIVITY",
 goal = [
QUOTE "\n!x y z. x = y /\\ y = z ==> x = z"]},

{name = "TRANS_SYMM",
 goal = [
QUOTE "\n!x y z. x = y /\\ y = z ==> z = x"]},

{name = "SUBSTITUTIVITY",
 goal = [
QUOTE "\n!x y. f x /\\ x = y ==> f y"]},

{name = "CYCLIC_SUBSTITUTION_BUG",
 goal = [
QUOTE "\n(!x. y = g (c x)) ==> ?z. y = g z"]},

(* ------------------------------------------------------------------------- *)
(* Simple equality problems.                                                 *)
(* ------------------------------------------------------------------------- *)

{name = "P48",
 goal = [
QUOTE "\n(a = b \\/ c = d) /\\ (a = c \\/ b = d) ==> a = d \\/ b = c"]},

{name = "P49",
 goal = [
QUOTE "\n(?x y. !z. z = x \\/ z = y) /\\ p a /\\ p b /\\ ~(a = b) ==> !x. p x"]},

{name = "P51",
 goal = [

QUOTE "\n(?z w. !x y. f0 x y <=> x = z /\\ y = w) ==>\n?z. !x. (?w. !y. f0 x y <=> y = w) <=> x = z"]},

{name = "P52",
 goal = [

QUOTE "\n(?z w. !x y. f0 x y <=> x = z /\\ y = w) ==>\n?w. !y. (?z. !x. f0 x y <=> x = z) <=> y = w"]},

(* ------------------------------------------------------------------------- *)
(* The Melham problem after an inverse skolemization step.                   *)
(* ------------------------------------------------------------------------- *)

{name = "UNSKOLEMIZED_MELHAM",
 goal = [
QUOTE "\n(!x y. g x = g y ==> f x = f y) ==> !y. ?w. !x. y = g x ==> w = f x"]},

(* ------------------------------------------------------------------------- *)
(* The example always given for congruence closure.                          *)
(* ------------------------------------------------------------------------- *)

{name = "CONGRUENCE_CLOSURE_EXAMPLE",
 goal = [
QUOTE "\n!x. f (f (f (f (f x)))) = x /\\ f (f (f x)) = x ==> f x = x"]},

(* ------------------------------------------------------------------------- *)
(* A simple example (see EWD1266a and the application to Morley's theorem).  *)
(* ------------------------------------------------------------------------- *)

{name = "EWD",
 goal = [

QUOTE "\n(!x. f x ==> g x) /\\ (?x. f x) /\\ (!x y. g x /\\ g y ==> x = y) ==>\n!y. g y ==> f y"]},

{name = "EWD'",
 goal = [
QUOTE "\n(!x. f (f x) = f x) /\\ (!x. ?y. f y = x) ==> !x. f x = x"]},

(* ------------------------------------------------------------------------- *)
(* Wishnu Prasetya's example.                                                *)
(* ------------------------------------------------------------------------- *)

{name = "WISHNU",
 goal = [

QUOTE "\n(?x. x = f (g x) /\\ !x'. x' = f (g x') ==> x = x') <=>\n?y. y = g (f y) /\\ !y'. y' = g (f y') ==> y = y'"]},

(* ------------------------------------------------------------------------- *)
(* An equality version of the Agatha puzzle.                                 *)
(* ------------------------------------------------------------------------- *)

{name = "AGATHA",
 goal = [









QUOTE "\n(?x. lives x /\\ killed x agatha) /\\\n(lives agatha /\\ lives butler /\\ lives charles) /\\\n(!x. lives x ==> x = agatha \\/ x = butler \\/ x = charles) /\\\n(!x y. killed x y ==> hates x y) /\\ (!x y. killed x y ==> ~richer x y) /\\\n(!x. hates agatha x ==> ~hates charles x) /\\\n(!x. ~(x = butler) ==> hates agatha x) /\\\n(!x. ~richer x agatha ==> hates butler x) /\\\n(!x. hates agatha x ==> hates butler x) /\\ (!x. ?y. ~hates x y) /\\\n~(agatha = butler) ==>\nkilled agatha agatha /\\ ~killed butler agatha /\\ ~killed charles agatha"]},

(* ------------------------------------------------------------------------- *)
(* Group theory examples.                                                    *)
(* ------------------------------------------------------------------------- *)

(* JRH: (Size 18, 61814 seconds.) *)
{name = "GROUP_RIGHT_INVERSE",
 goal = [

QUOTE "\n(!x y z. x * (y * z) = x * y * z) /\\ (!x. e * x = x) /\\\n(!x. i x * x = e) ==> !x. x * i x = e"]},

{name = "GROUP_RIGHT_IDENTITY",
 goal = [

QUOTE "\n(!x y z. x * (y * z) = x * y * z) /\\ (!x. e * x = x) /\\\n(!x. i x * x = e) ==> !x. x * e = x"]},

{name = "KLEIN_GROUP_COMMUTATIVE",
 goal = [

QUOTE "\n(!x y z. x * (y * z) = x * y * z) /\\ (!x. e * x = x) /\\ (!x. x * e = x) /\\\n(!x. x * x = e) ==> !x y. x * y = y * x"]}

];

(* ========================================================================= *)
(* Some sample problems from the TPTP archive.                               *)
(* Note: for brevity some relation/function names have been shortened.       *)
(* ========================================================================= *)

val tptp = [

(* ------------------------------------------------------------------------- *)
(* TPTP problems that have demonstrated bugs in the prover.                  *)
(* ------------------------------------------------------------------------- *)

(* Solved trivially by meson without cache cutting, but not with. *)
{name = "PUZ011-1",
 goal = [











QUOTE "\nocean atlantic /\\ ocean indian /\\ borders atlantic brazil /\\\nborders atlantic uruguay /\\ borders atlantic c_venesuela /\\\nborders atlantic c_zaire /\\ borders atlantic nigeria /\\\nborders atlantic angola /\\ borders indian india /\\\nborders indian pakistan /\\ borders indian iran /\\ borders indian somalia /\\\nborders indian kenya /\\ borders indian tanzania /\\ south_american brazil /\\\nsouth_american uruguay /\\ south_american c_venesuela /\\ african c_zaire /\\\nafrican nigeria /\\ african angola /\\ african somalia /\\ african kenya /\\\nafrican tanzania /\\ asian india /\\ asian pakistan /\\ asian iran ==>\n(!x y z.\n   ~ocean x \\/ ~borders x y \\/ ~african y \\/ ~borders x z \\/ ~asian z) ==>\nF"]},

(* ------------------------------------------------------------------------- *)
(* Problems used by the fol unit test to exercise the TPTP parser.           *)
(* ------------------------------------------------------------------------- *)

{name = "PUZ001-1",
 goal = [








QUOTE "\nlives agatha /\\ lives butler /\\ lives charles /\\\n(!x y. ~killed x y \\/ ~richer x y) /\\\n(!x. ~hates agatha x \\/ ~hates charles x) /\\\n(!x. ~hates x agatha \\/ ~hates x butler \\/ ~hates x charles) /\\\nhates agatha agatha /\\ hates agatha charles /\\\n(!x y. ~killed x y \\/ hates x y) /\\\n(!x. ~hates agatha x \\/ hates butler x) /\\\n(!x. ~lives x \\/ richer x agatha \\/ hates butler x) ==>\nkilled butler agatha \\/ killed charles agatha ==> F"]},

{name = "PUZ020-1",
 goal = [
























QUOTE "\n(!x. x = x) /\\ (!x y. ~(x = y) \\/ y = x) /\\\n(!x y z. ~(x = y) \\/ ~(y = z) \\/ x = z) /\\\n(!x y. ~(x = y) \\/ statement_by x = statement_by y) /\\\n(!x. ~person x \\/ knight x \\/ knave x) /\\\n(!x. ~person x \\/ ~knight x \\/ ~knave x) /\\\n(!x y. ~says x y \\/ a_truth y \\/ ~a_truth y) /\\\n(!x y. ~says x y \\/ ~(x = y)) /\\ (!x y. ~says x y \\/ y = statement_by x) /\\\n(!x y. ~person x \\/ ~(x = statement_by y)) /\\\n(!x. ~person x \\/ ~a_truth (statement_by x) \\/ knight x) /\\\n(!x. ~person x \\/ a_truth (statement_by x) \\/ knave x) /\\\n(!x y. ~(x = y) \\/ ~knight x \\/ knight y) /\\\n(!x y. ~(x = y) \\/ ~knave x \\/ knave y) /\\\n(!x y. ~(x = y) \\/ ~person x \\/ person y) /\\\n(!x y z. ~(x = y) \\/ ~says x z \\/ says y z) /\\\n(!x y z. ~(x = y) \\/ ~says z x \\/ says z y) /\\\n(!x y. ~(x = y) \\/ ~a_truth x \\/ a_truth y) /\\\n(!x y. ~knight x \\/ ~says x y \\/ a_truth y) /\\\n(!x y. ~knave x \\/ ~says x y \\/ ~a_truth y) /\\ person husband /\\\nperson c_wife /\\ ~(husband = c_wife) /\\\nsays husband (statement_by husband) /\\\n(~a_truth (statement_by husband) \\/ ~knight husband \\/ knight c_wife) /\\\n(a_truth (statement_by husband) \\/ ~knight husband) /\\\n(a_truth (statement_by husband) \\/ knight c_wife) /\\\n(~knight c_wife \\/ a_truth (statement_by husband)) ==> ~knight husband ==>\nF"]},

{name = "NUM001-1",
 goal = [








QUOTE "\n(!x. x == x) /\\ (!x y z. ~(x == y) \\/ ~(y == z) \\/ x == z) /\\\n(!x y. x + y == y + x) /\\ (!x y z. x + (y + z) == x + y + z) /\\\n(!x y. x + y - y == x) /\\ (!x y. x == x + y - y) /\\\n(!x y z. x - y + z == x + z - y) /\\ (!x y z. x + y - z == x - z + y) /\\\n(!x y z v. ~(x == y) \\/ ~(z == x + v) \\/ z == y + v) /\\\n(!x y z v. ~(x == y) \\/ ~(z == v + x) \\/ z == v + y) /\\\n(!x y z v. ~(x == y) \\/ ~(z == x - v) \\/ z == y - v) /\\\n(!x y z v. ~(x == y) \\/ ~(z == v - x) \\/ z == v - y) ==>\n~(a + b + c == a + (b + c)) ==> F"]},

{name = "ALG005-1",
 goal = [








QUOTE "\n(!x. x = x) /\\ (!x y. ~(x = y) \\/ y = x) /\\\n(!x y z. ~(x = y) \\/ ~(y = z) \\/ x = z) /\\\n(!x y z. ~(x = y) \\/ x + z = y + z) /\\\n(!x y z. ~(x = y) \\/ z + x = z + y) /\\\n(!x y z. ~(x = y) \\/ x * z = y * z) /\\\n(!x y z. ~(x = y) \\/ z * x = z * y) /\\ (!x y. x + (y + x) = x) /\\\n(!x y. x + (x + y) = y + (y + x)) /\\\n(!x y z. x + y + z = x + z + (y + z)) /\\ (!x y. x * y = x + (x + y)) ==>\n~(a * b * c = a * (b * c)) ==> F"]},

{name = "GRP057-1",
 goal = [






QUOTE "\n(!x. x = x) /\\ (!x y. ~(x = y) \\/ y = x) /\\\n(!x y z. ~(x = y) \\/ ~(y = z) \\/ x = z) /\\\n(!x y z v. x * i (i (i y * (i x * z)) * v * i (y * v)) = z) /\\\n(!x y. ~(x = y) \\/ i x = i y) /\\ (!x y z. ~(x = y) \\/ x * z = y * z) /\\\n(!x y z. ~(x = y) \\/ z * x = z * y) ==>\n~(i a1 * a1 = i b1 * b1) \\/ ~(i b2 * b2 * a2 = a2) \\/\n~(a3 * b3 * c3 = a3 * (b3 * c3)) ==> F"]},

{name = "LCL009-1",
 goal = [


QUOTE "\n(!x y. ~p (x - y) \\/ ~p x \\/ p y) /\\\n(!x y z. p (x - y - (z - y - (x - z)))) ==>\n~p (a - b - c - (a - (b - c))) ==> F"]},

(* ------------------------------------------------------------------------- *)
(* Small problems that are tricky to prove.                                  *)
(* ------------------------------------------------------------------------- *)

{name = "COL060-3",
 goal = [





QUOTE "\n(!x. x = x) /\\ (!x y. ~(x = y) \\/ y = x) /\\\n(!x y z. ~(x = y) \\/ ~(y = z) \\/ x = z) /\\\n(!x y z. b % x % y % z = x % (y % z)) /\\ (!x y. t % x % y = y % x) /\\\n(!x y z. ~(x = y) \\/ x % z = y % z) /\\\n(!x y z. ~(x = y) \\/ z % x = z % y) ==>\n~(b % (b % (t % b) % b) % t % c_x % c_y % c_z = c_y % (c_x % c_z)) ==> F"]},

{name = "COL058-2",
 goal = [






QUOTE "\n(!x. x = x) /\\ (!x y. ~(x = y) \\/ y = x) /\\\n(!x y z. ~(x = y) \\/ ~(y = z) \\/ x = z) /\\\n(!x y. r (r 0 x) y = r x (r y y)) /\\ (!x y z. ~(x = y) \\/ r x z = r y z) /\\\n(!x y z. ~(x = y) \\/ r z x = r z y) ==>\n~(r (r (r 0 (r (r 0 (r 0 0)) (r 0 (r 0 0)))) (r 0 (r 0 0)))\n  (r (r 0 (r (r 0 (r 0 0)) (r 0 (r 0 0)))) (r 0 (r 0 0))) =\n  r (r 0 (r (r 0 (r 0 0)) (r 0 (r 0 0)))) (r 0 (r 0 0))) ==> F"]},

{name = "LCL107-1",
 goal = [




QUOTE "\n(!x y. ~p (x - y) \\/ ~p x \\/ p y) /\\\n(!x y z v w x' y'.\n   p\n   (x - y - z - (v - w - (x' - w - (x' - v) - y')) -\n    (z - (y - x - y')))) ==> ~p (a - b - c - (e - b - (a - e - c))) ==> F"]},

{name = "LDA007-3",
 goal = [






QUOTE "\n(!x. x = x) /\\ (!x y. ~(x = y) \\/ y = x) /\\\n(!x y z. ~(x = y) \\/ ~(y = z) \\/ x = z) /\\\n(!x y z. f x (f y z) = f (f x y) (f x z)) /\\\n(!x y z. ~(x = y) \\/ f x z = f y z) /\\\n(!x y z. ~(x = y) \\/ f z x = f z y) /\\ tt = f t t /\\ ts = f t s /\\\ntt_ts = f tt ts /\\ tk = f t k /\\ tsk = f ts k ==>\n~(f t tsk = f tt_ts tk) ==> F"]},

{name = "GRP010-4",
 goal = [




QUOTE "\n(!x. x = x) /\\ (!x y. ~(x = y) \\/ y = x) /\\\n(!x y z. ~(x = y) \\/ ~(y = z) \\/ x = z) /\\ (!x y. ~(x = y) \\/ i x = i y) /\\\n(!x y z. ~(x = y) \\/ x * z = y * z) /\\\n(!x y z. ~(x = y) \\/ z * x = z * y) /\\ (!x y z. x * y * z = x * (y * z)) /\\\n(!x. 1 * x = x) /\\ (!x. i x * x = 1) /\\ c * b = 1 ==> ~(b * c = 1) ==> F"]},

{name = "ALG006-1",
 goal = [





QUOTE "\n(!x. x = x) /\\ (!x y. ~(x = y) \\/ y = x) /\\\n(!x y z. ~(x = y) \\/ ~(y = z) \\/ x = z) /\\\n(!x y z. ~(x = y) \\/ x + z = y + z) /\\\n(!x y z. ~(x = y) \\/ z + x = z + y) /\\ (!x y. x + (y + x) = x) /\\\n(!x y. x + (x + y) = y + (y + x)) /\\\n(!x y z. x + y + z = x + z + (y + z)) ==> ~(a + c + b = a + b + c) ==> F"]},

{name = "BOO021-1",
 goal = [








QUOTE "\n(!x. x = x) /\\ (!x y. ~(x = y) \\/ y = x) /\\\n(!x y z. ~(x = y) \\/ ~(y = z) \\/ x = z) /\\\n(!x y z. ~(x = y) \\/ x + z = y + z) /\\\n(!x y z. ~(x = y) \\/ z + x = z + y) /\\ (!x y. ~(x = y) \\/ i x = i y) /\\\n(!x y z. ~(x = y) \\/ x * z = y * z) /\\\n(!x y z. ~(x = y) \\/ z * x = z * y) /\\ (!x y. (x + y) * y = y) /\\\n(!x y z. x * (y + z) = y * x + z * x) /\\ (!x. x + i x = 1) /\\\n(!x y. x * y + y = y) /\\ (!x y z. x + y * z = (y + x) * (z + x)) /\\\n(!x. x * i x = 0) ==> ~(b * a = a * b) ==> F"]},

{name = "GEO002-4",
 goal = [











QUOTE "\n(!x y z v. ~between x y z \\/ ~between y v z \\/ between x y v) /\\\n(!x y z. ~equidistant x y z z \\/ x == y) /\\\n(!x y z v w.\n   ~between x y z \\/ ~between v z w \\/\n   between x (outer_pasch y x v w z) v) /\\\n(!x y z v w.\n   ~between x y z \\/ ~between v z w \\/\n   between w y (outer_pasch y x v w z)) /\\\n(!x y z v. between x y (extension x y z v)) /\\\n(!x y z v. equidistant x (extension y x z v) z v) /\\\n(!x y z v. ~(x == y) \\/ ~between z v x \\/ between z v y) ==>\n~between a a b ==> F"]},

{name = "GRP057-1",
 goal = [






QUOTE "\n(!x. x = x) /\\ (!x y. ~(x = y) \\/ y = x) /\\\n(!x y z. ~(x = y) \\/ ~(y = z) \\/ x = z) /\\\n(!x y z v. x * i (i (i y * (i x * z)) * v * i (y * v)) = z) /\\\n(!x y. ~(x = y) \\/ i x = i y) /\\ (!x y z. ~(x = y) \\/ x * z = y * z) /\\\n(!x y z. ~(x = y) \\/ z * x = z * y) ==>\n~(i a1 * a1 = i b1 * b1) \\/ ~(i b2 * b2 * a2 = a2) \\/\n~(a3 * b3 * c3 = a3 * (b3 * c3)) ==> F"]},

{name = "HEN006-3",
 goal = [









QUOTE "\n(!x. x = x) /\\ (!x y. ~(x = y) \\/ y = x) /\\\n(!x y z. ~(x = y) \\/ ~(y = z) \\/ x = z) /\\\n(!x y. ~(x <= y) \\/ x / y = 0) /\\ (!x y. ~(x / y = 0) \\/ x <= y) /\\\n(!x y. x / y <= x) /\\ (!x y z. x / y / (z / y) <= x / z / y) /\\\n(!x. 0 <= x) /\\ (!x y. ~(x <= y) \\/ ~(y <= x) \\/ x = y) /\\ (!x. x <= 1) /\\\n(!x y z. ~(x = y) \\/ x / z = y / z) /\\\n(!x y z. ~(x = y) \\/ z / x = z / y) /\\\n(!x y z. ~(x = y) \\/ ~(x <= z) \\/ y <= z) /\\\n(!x y z. ~(x = y) \\/ ~(z <= x) \\/ z <= y) /\\ a / b <= d ==>\n~(a / d <= b) ==> F"]},

{name = "RNG035-7",
 goal = [











QUOTE "\n(!x. x = x) /\\ (!x y. ~(x = y) \\/ y = x) /\\\n(!x y z. ~(x = y) \\/ ~(y = z) \\/ x = z) /\\ (!x. 0 + x = x) /\\\n(!x. x + 0 = x) /\\ (!x. n x + x = 0) /\\ (!x. x + n x = 0) /\\\n(!x y z. x + (y + z) = x + y + z) /\\ (!x y. x + y = y + x) /\\\n(!x y z. x * (y * z) = x * y * z) /\\\n(!x y z. x * (y + z) = x * y + x * z) /\\\n(!x y z. (x + y) * z = x * z + y * z) /\\\n(!x y z. ~(x = y) \\/ x + z = y + z) /\\\n(!x y z. ~(x = y) \\/ z + x = z + y) /\\ (!x y. ~(x = y) \\/ n x = n y) /\\\n(!x y z. ~(x = y) \\/ x * z = y * z) /\\\n(!x y z. ~(x = y) \\/ z * x = z * y) /\\ (!x. x * (x * (x * x)) = x) ==>\na * b = c /\\ ~(b * a = c) ==> F"]},

{name = "ROB001-1",
 goal = [






QUOTE "\n(!x. x = x) /\\ (!x y. ~(x = y) \\/ y = x) /\\\n(!x y z. ~(x = y) \\/ ~(y = z) \\/ x = z) /\\ (!x y. x + y = y + x) /\\\n(!x y z. x + y + z = x + (y + z)) /\\\n(!x y. n (n (x + y) + n (x + n y)) = x) /\\\n(!x y z. ~(x = y) \\/ x + z = y + z) /\\\n(!x y z. ~(x = y) \\/ z + x = z + y) /\\ (!x y. ~(x = y) \\/ n x = n y) ==>\n~(n (a + n b) + n (n a + n b) = b) ==> F"]},

{name = "GRP128-4.003",
 goal = [
















QUOTE "\n(!x y.\n   ~elt x \\/ ~elt y \\/ product e_1 x y \\/ product e_2 x y \\/\n   product e_3 x y) /\\\n(!x y.\n   ~elt x \\/ ~elt y \\/ product x e_1 y \\/ product x e_2 y \\/\n   product x e_3 y) /\\ elt e_1 /\\ elt e_2 /\\ elt e_3 /\\ ~(e_1 == e_2) /\\\n~(e_1 == e_3) /\\ ~(e_2 == e_1) /\\ ~(e_2 == e_3) /\\ ~(e_3 == e_1) /\\\n~(e_3 == e_2) /\\\n(!x y.\n   ~elt x \\/ ~elt y \\/ product x y e_1 \\/ product x y e_2 \\/\n   product x y e_3) /\\\n(!x y z v. ~product x y z \\/ ~product x y v \\/ z == v) /\\\n(!x y z v. ~product x y z \\/ ~product x v z \\/ y == v) /\\\n(!x y z v. ~product x y z \\/ ~product v y z \\/ x == v) ==>\n(!x y z v. product x y z \\/ ~product x z v \\/ ~product z y v) /\\\n(!x y z v. product x y z \\/ ~product v x z \\/ ~product v y x) /\\\n(!x y z v. ~product x y z \\/ ~product z y v \\/ product x z v) ==> F"]},

{name = "NUM014-1",
 goal = [






QUOTE "\n(!x. product x x (square x)) /\\\n(!x y z. ~product x y z \\/ product y x z) /\\\n(!x y z. ~product x y z \\/ divides x z) /\\\n(!x y z v.\n   ~prime x \\/ ~product y z v \\/ ~divides x v \\/ divides x y \\/\n   divides x z) /\\ prime a /\\ product a (square c) (square b) ==>\n~divides a b ==> F"]}

];

(* ========================================================================= *)
(* A FEW SAMPLE THEOREMS TO CHECK LARGE RUNS                                 *)
(* ========================================================================= *)

(* val quick =
 *   [extract nonequality "TRUE",
 *    extract nonequality "P_or_not_P",
 *    extract nonequality "JH_test",
 *    extract nonequality "CYCLIC",
 *    extract nonequality "MN_bug",
 *    extract nonequality "ERIC",
 *    extract nonequality "MATHS4_EXAMPLE",
 *    extract nonequality "P18",
 *    extract nonequality "P39",
 *    extract nonequality "P59",
 *    extract nonequality "DAVIS_PUTNAM_EXAMPLE",
 *    extract nonequality "BAD_CONNECTIONS",
 *
 *    extract equality "TRANS_SYMM",
 *    extract equality "CYCLIC_SUBSTITUTION_BUG",
 *    extract equality "P48"];
 *)
end
