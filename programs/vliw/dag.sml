(* dag.sml
 *)

structure Dag :
    sig
        exception DAG
        exception DAGnotfound
        type dag
        val make : dag
        val tests_of : dag -> Ntypes.test Set.set
        val sel_of : dag -> ((Ntypes.test * bool) -> Ntypes.test_or_name)
        val root_of : dag -> Ntypes.test_or_name
        val succ_of : dag -> Ntypes.name Set.set
        val attach : Ntypes.test * dag * dag -> dag
        val reach : dag * Ntypes.test_or_name -> dag
        val replace_edge : dag * Ntypes.name list -> dag
        val newdag : (Ntypes.test Set.set *
                      ((Ntypes.test * bool) -> Ntypes.test_or_name) *
                      Ntypes.test_or_name *
                      Ntypes.name Set.set)
            -> dag
        val dagToString : dag -> string
    end =
struct

open Ntypes;


exception DAGnotfound
exception DAG

datatype dag =
    D of
    test Set.set *
    ((test * bool) -> test_or_name) *
    test_or_name *
    name Set.set

fun tonToString (TEST t) = "TEST t"
  | tonToString (NAME n) = "NAME " ^ n
  | tonToString NEITHER = "NEITHER"

fun sep (a, b) = a ^ ", " ^ b

fun dagToString (D(t, sel, rt, s)) =
    "D([" ^ PrintAbs.str (Set.set t) ^ "]" ^
    "fn, " ^ (tonToString rt) ^ ", " ^ (List.foldr sep ")" (Set.set s))

val make = D(Set.makeEQ teq, fn x => raise DAGnotfound, NEITHER, Set.make)

fun newdag x = D x

fun tests_of(D (b, sel, r, h)) = b
fun sel_of(D (b, sel, r, h)) = sel
fun root_of(D (b, sel, r, h)) = r
fun succ_of(D (b, sel, r, h)) = h

fun attach (t, D dt, D df) =
    let open Set
        val (b1, sel1, r1, h1) = dt
        val (b2, sel2, r2, h2) = df
    in
        D(add(union(b1, b2), t),
          (fn(x, y) =>
           if teq(x, t) then if y then r1 else r2
           else sel1(x, y) handle DAGnotfound => sel2(x, y)),
          TEST t,
          union(h1,h2)
          )
    end

fun reach (D d, tn) =
    let open Set
        val (b, sel, r, h) = d
        fun f (TEST t) =
            if not (member(b, t)) then raise DAGnotfound
            else attach(t, reach(D d, sel(t, true)), reach(D d, sel(t, false)))
         | f (NAME n) =
           D(makeEQ teq, fn x => raise DAGnotfound, NAME n, listToSet [n])
           | f (_) = raise DAGnotfound
    in
        f tn
    end

fun replace_edge (D d, nil) = D d
  | replace_edge (D d, old::new::tl) =
    let open Set
        val (b, sel, r, h)  = d
        val nh = if member(h, old) then add(rm(h, old), new) else h
        val nr = if toneq(r, NAME old) then NAME new else r
        val nsel = fn(x, y) =>
            let val v = sel(x, y)
            in
                if toneq(v,  NAME old) then NAME new else v
            end
    in
        D (b, nsel, nr, nh)
    end
  | replace_edge _ = raise DAG

end
