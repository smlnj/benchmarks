(* compress.sml
 *)

structure Compress :
    sig
        val compress_debug : bool ref
        val compress : (int * Node.program) -> Node.program
        val move_things_node :
            Node.program * Ntypes.name * Ntypes.name Set.set -> Node.program
        val do_move_tests : bool ref
        val do_move_ops : bool ref

        val dbg_p : Node.program ref

    end =

struct

open Ntypes
open Dag
open Node

val do_move_tests = ref false
val do_move_ops = ref true

exception COMPRESS

fun error (s:string) =
    (print (s ^ "\n");
     raise COMPRESS)

val compress_debug = ref false

val dbg_p =  ref (makeProg())

type debug_fun = unit -> string
fun debug (f:debug_fun) =
    if !compress_debug then print (f() ^ "\n")
    else ()

exception FILTERSUCC

fun filterSucc(P, nm, fence_set) =
    let open Set
        val s = set(succ(P, nameToNode(P, nm)))
            handle NAMETONODE => raise FILTERSUCC
        fun f (nm, l) = if member(fence_set, nm) then l else nm::l
    in
        List.foldr f nil s
    end

(*
val inP = ref false
val finP = ref makeProg
val foutP = ref makeProg

fun chinP (p, from) =
    let val nm = "11_100'_110tt_119'"
        val prd = prednm(p, nm)
        val pe = Set.empty(prd)
    in
        if !inP then
            if pe then (foutP := p; error ("chinP gone -" ^ from)) else ()
        else if pe then ()
             else (inP := true;
                   print ("chinP found it -" ^ from ^ "\n");
                   finP := p;
                   nameToNode(p, nm);
                   ())
    end
*)

exception MOVETHINGSNODE
fun move_things_node(P, nm, fence_set) =
    let open Set
        (*
        val foo = debug
            (fn () =>
            "move_things_node(\n" ^
            progToString P ^ ",\n" ^
            nm ^ ", [" ^
            fold (fn (a, b) => a ^ ", " ^ b) "]" (set fence_set) ^
            ")")
            *)
        fun ntn (p, nm) = ((* chinP (p, "ntn");*) nameToNode (p, nm))
            handle NAMETONODE => (dbg_p := P; raise MOVETHINGSNODE)
        fun s_nm_list p = filterSucc(p, nm, fence_set)
        fun nd nm = ntn(P, nm) handle MOVETHINGSNODE => error "nd  nm"
        val au = listUnionEQ(aeq, map (assignment_of o nd) (s_nm_list P))
        val tu = listUnionEQ(teq, map (tests_of o dag_of o nd) (s_nm_list P))
        fun ms (p, a) =
            let fun f(nm, l) =
                ((*chinP (p, "ms"); *)
                 if member(assignment_of(ntn(p, nm)), a) then nm::l
                 else l
                     )
                handle MOVETHINGSNODE => (dbg_p := p; error "ms")
            in
                List.foldr f nil (s_nm_list p)
            end
        fun move_a1(a, p) =
            let val msl = ms (p, a)
                val ms_set = nameSetToNodeSet(p, listToSet msl)
                fun dms(a, p) = delete(p, ntn(p, a))
                fun mop() =
                    let val foo = debug (fn () => "mop start " ^ nm)
                        val new_p = move_op(p, a, ms_set, ntn(p, nm))
                            handle MOVETHINGSNODE => error "move_a move_op"
                        val foo = debug (fn () => "mop end")
                    in
                        new_p
                    end
                val mpa = mop()
                    (*
                val foo = chinP(mpa,
                                "a_move_a amop " ^ nm ^
                                StrPak.stringListString
                                (map name_of (set ms_set)))
                     *)
                val answer = List.foldr dms mpa msl
                    (*
                val foo = chinP(answer, "a_move_a adel")
                     *)
            in
                answer
            end
        fun move_a(a, p) = if !do_move_ops then move_a1(a, p) else p
        fun tset (p, t) =
            let fun f(nm, l) =
                ((*chinP (p, "tset");*)
                 if member(tests_of(dag_of(ntn(p, nm))), t) then nm::l
                 else l
                     )
                handle MOVETHINGSNODE => error "tset"
            in
                List.foldr f nil (s_nm_list p)
            end
        fun move_t1(t, p) =
            let val ts = tset (p, t)
                val answer =
                    if List.length ts > 0 then
                        move_test(p, t,
                                  (ntn(p, hd ts)
                                   handle MOVETHINGSNODE => error "move_t 1"),
                                  (ntn(p, nm)
                                   handle MOVETHINGSNODE => error "move_t 2"))

                    else p
                (*val foo = chinP(answer, "a_move_t")*)
            in
                answer
            end
        fun move_t(t, p) = if !do_move_tests then move_t1(t, p) else p
    in
        debug (fn () => "movethingsnode " ^ nm ^ "\n");
        List.foldr move_t (List.foldr move_a P (set au)) (set tu)
    end

exception MOVETHINGSWINDOW
fun move_things_window(P, w, nm, fence_set) =
    let open Set
        (*
        val foo = debug (fn () =>
                         "move_things_window(\n" ^
                         progToString P ^ ",\n" ^
                         (Int.toString w) ^ ", " ^
                         nm ^ ", [" ^
                         fold (fn (a, b) => a ^ ", " ^ b) (set fence_set) "]" ^
                         ")\n")
            *)
        fun ntn (P, nm) = (nameToNode (P, nm))
            handle NAMETONODE =>  raise MOVETHINGSWINDOW
        val node = ntn(P, nm)
        val things = num_things_node node
        val s_nm_list = filterSucc(P, nm, fence_set)
        fun nxt(nm, p) =
            move_things_window(p, w - things, nm, fence_set)
        val child_p = if w > things then List.foldr nxt P s_nm_list else P
    in
        debug (fn () => "movethingswindow " ^ nm ^ "\n");
        move_things_node(child_p, nm, fence_set)
    end


exception CPRESS
exception CPRESS1
exception CPRESS2
exception CPRESS3
exception CPRESS4
exception CPRESS5
fun cpress(window, P, fence_set, everin_fence_set) =
    let open Set
        fun nxt(nm, p:program) =
            ((* dbg_p := p; *)
             move_things_window(p, window, nm, fence_set))
            handle MOVETHINGSWINDOW => raise CPRESS1
        val filled = List.foldr nxt P (set fence_set)
            handle CPRESS1 => raise CPRESS2
        fun succf nm = succ(filled, nameToNode(filled, nm))
            handle NAMETONODE => raise CPRESS
        val nfence_set = listUnion(make::(map succf (set fence_set)))
        fun filt(a, l) = if member(everin_fence_set, a) then l else a::l
        val f_fence_set = listToSet(List.foldr filt nil (set nfence_set))
        val n_everin_fc =
            List.foldr (fn (a, s) => add(s, a)) everin_fence_set (set f_fence_set)
    in
        debug (fn () => "cpress: fence_set=" ^
               StrPak.stringListString (set fence_set) ^
               "\n f_fence_set =" ^ StrPak.stringListString (set f_fence_set));
        if not (empty f_fence_set)
            then cpress(window, filled, f_fence_set, n_everin_fc)
                handle CPRESS => raise CPRESS3
                    handle CPRESS1 => raise CPRESS4
                        handle CPRESS2 => raise CPRESS5
        else filled
    end

fun clean_up (P as (ns, n0, F):program) =
    let        val foo = debug (fn () => "cleanup")
        val clos = closure(P, n0)
        val (ns, n0, F) = clos
        val l = (map name_of (Stringmap.extract ns))
        fun f (n, p) =
            (debug (fn () => "cleanup deleting " ^ n);
            delete(p, nameToNode(p, n)))
        val answer = List.foldr f clos l
        val foo = debug (fn () => "exiting cleanup")
    in
        answer
    end

fun compress(window, P as (ns, n0, F)) =
    let open Set
        val fence = n0
        val fence_set = add(make, name_of n0)
        val everin_fence_set = add(makeEQ(name_prefix_eq), name_of n0)
        val uc = cpress(window, P, fence_set, everin_fence_set)
        val cu = clean_up uc
    in
        debug (fn () => "compress");
        cu
    end

end
