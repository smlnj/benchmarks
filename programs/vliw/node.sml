(* node.sml
 *)

structure Node :
    sig
        type node
        type program
        val delete_debug : bool ref
        val move_op_debug : bool ref
        val move_test_debug : bool ref
        val rw_debug : bool ref
        val ntn_debug : bool ref
        val prog_node_debug : bool ref
        val prog_node_debug_verbose : bool ref
        val closure_progs_debug : bool ref
        val cpsiCheck : bool ref
        val makeProg : unit -> program
        val make :
            Ntypes.name * Ntypes.assignment Set.set *
            Dag.dag * Ntypes.name Set.set-> node
        val name_of : node -> Ntypes.name
        val assignment_of : node -> Ntypes.assignment Set.set
        val dag_of : node -> Dag.dag
        val succ : program * node -> Ntypes.name Set.set
        val prednm : program * Ntypes.name -> Ntypes.name Set.set
        val pred : program * node -> Ntypes.name Set.set
        val succNodes : program * node -> node Set.set
        val predNodes : program * node -> node Set.set
        val readNode : node -> int Set.set
        val writeNode : node -> int Set.set
        val unreachable : program * node -> bool
        val num_ops_node : node -> int
        val num_tests_node : node -> int
        val num_things_node : node -> int
        val replace_edge_node : node * string list -> node
        exception NAMETONODE
        val nameToNode : program * Ntypes.name -> node
        val nameSetToNodeSet : program * Ntypes.name Set.set -> node Set.set
        val eqn : node * node -> bool
        val n00 : node
        val fin : node
        val delete : program * node -> program
        val move_op :
            program * Ntypes.assignment * node Set.set * node -> program
        val move_test :  program * Ntypes.test * node * node -> program
        val nodeToString : node -> string
        val progToString : program -> string
        val entries : program -> node list
        val programs : program -> program list
        val addPredInfo : program -> program
        val closure : program * node -> program
        val sortNodes : node list -> node list
        val updateNode : program * node -> program
        val addNode : program * node -> program
        val rmNode : program * node -> program
    end =
struct

open Ntypes
open Dag
open StrPak
datatype node = N of name * assignment Set.set * dag * name Set.set
type program = node Stringmap.stringmap * node * node

type debug_fun = unit -> string
val delete_debug = ref false
val move_op_debug = ref false
val dead_set_debug = ref false
val move_test_debug = ref false
val rw_debug = ref false
val prog_node_debug = ref false
val prog_node_debug_verbose = ref false
val closure_progs_debug = ref false

fun name_of(N(n, a, d, prd)) = n
fun assignment_of(N(n, a, d, prd)) = a
fun dag_of(N(n, a, d, prd)) = d
fun pred_of(N(n, a, d, prd)) = prd

fun eqn(n1, n2) = name_of n1 = name_of n2

val start:name = "START"
val finish:name = "FINISH"

fun printstringlist sl = stringListString sl
val psl = printstringlist

fun nodeToString (N(n, a, d, prd)) =
    "\nN(" ^ n ^ ", [" ^ PrintAbs.str (Set.set a) ^ "], " ^
    Dag.dagToString d ^
    "pred(" ^ psl (Set.set prd) ^ "))"

fun progToString (ns, n0, F) =
    "P (" ^ (psl o (map nodeToString) o Stringmap.extract) ns ^ ",\n" ^
    nodeToString n0 ^ ",\n" ^
    nodeToString F ^ ")\n"

fun make (n, a, t, prd) = N(n, a, t, prd)

val n00 = make(start, Set.makeEQ aeq, Dag.make, Set.make)
val fin = make(finish, Set.makeEQ aeq, Dag.make, Set.make)

fun makeProg() = (Stringmap.new():node Stringmap.stringmap, n00, fin)

fun addPredNode (N(n, a, t, prd), p) = (N(n, a, t, Set.add(prd, p)))
fun unionPredNode (N(n, a, t, prd), ps) = (N(n, a, t, Set.union(prd, ps)))
fun setPredNode (N(n, a, t, prd), p) = (N(n, a, t, p))
fun rmPredNode (N(n, a, t, prd), p) = (N(n, a, t, Set.rm(prd, p)))

fun p_n_debug (f:debug_fun) =
    if !prog_node_debug then print ("p_n:" ^ f() ^ "\n")
    else ()


fun updateNode(P as (ns, n0, F), new_node) =
    let val answer =
        (Stringmap.rm (ns:node Stringmap.stringmap)
         ((name_of new_node):string);
         Stringmap.add ns ((name_of new_node), new_node);
         if name_of new_node = name_of n0 then (ns, new_node, F)
         else if name_of new_node = name_of F then (ns, n0, new_node)
              else P)
        val foo = p_n_debug
            (fn () =>
             ("updateNode n=" ^ nodeToString new_node ^
              "=>" ^
                  (if !prog_node_debug_verbose then progToString answer
                   else "(program)")))
    in
        answer
    end

fun addNode(P as (ns, n0, F), new_node) =
    let val answer =
        if Stringmap.isin ns (name_of new_node) then updateNode(P, new_node)
        else (Stringmap.add ns ((name_of new_node), new_node);
              P)
        val foo = p_n_debug
            (fn () =>
             ("addNode n=" ^ nodeToString new_node ^
              "=>" ^
                  (if !prog_node_debug_verbose then progToString answer
                   else "(program)")))
    in
        answer
    end


fun rmNode(P as (ns, n0, F), node) =
    let val answer = (Stringmap.rm ns (name_of node);
                      P)
        val foo = p_n_debug
            (fn () =>
             ("rmNode n=" ^ nodeToString node ^
              "=>" ^
                  (if !prog_node_debug_verbose then progToString answer
                   else "(program)")))
    in
        answer
    end


fun succ(p, n) = (succ_of o dag_of) n
fun pred(p, n) = pred_of  n

val ntn_debug = ref true
fun ntnPrint (f:debug_fun) = if !ntn_debug then print ("ntn:" ^ f() ^ "\n") else ()

exception NAMETONODE
fun nameToNode(P as (ns, n0, F), nm) =
    Stringmap.map ns nm
    handle Stringmap =>
        (ntnPrint (fn () => ("nameToNode " ^ nm ^ "not found"));
         raise NAMETONODE)

exception NAMESETTONODESET
fun nameSetToNodeSet(P, ns) =
    Set.listToSetEQ(eqn, map (fn x => nameToNode(P, x)) (Set.set ns))
    handle NAMETONODE => raise NAMESETTONODESET

fun prednm(p, nm) = pred(p, nameToNode(p, nm))

fun succNodes (p, n) = nameSetToNodeSet(p, succ(p, n))
fun predNodes (p, n) = nameSetToNodeSet(p, pred(p, n))


(* a correctness assertion *)
exception CPSI
val cpsiCheck = ref false
fun checkPredSuccInfo(from, P as (ns, n0, F)) =
    let val nl = Stringmap.extract ns
        val badnode = ref n0
        fun fail s = (print ("CPSI:" ^ s ^ " failed\nfrom " ^ from ^
                             "\nbadnode=" ^ nodeToString (!badnode) ^
                             "\nprogram=" ^ progToString P ^ "\n");
                      raise CPSI)
        fun chk (xpred, xsuccN, n) =
            let val foo = badnode := n
                val s = Set.set(xsuccN(P, n))
                    handle NAMESETTONODESET =>
                        fail "NAMESETTONODESET"
                    fun cs x = Set.member(xpred x, name_of n)
                fun fs (x, b) = b andalso cs x
            in
                List.foldr fs true s
            end
        fun cp x = chk(pred_of, succNodes, x)
        fun cs x = chk((succ_of o dag_of), predNodes, x)
    in
        if not (List.all cp nl) then fail "cp"
        else if not (List.all cs nl) then fail "cs"
             else ()
    end
fun cpsi x = if !cpsiCheck then checkPredSuccInfo x else ()


fun empty n =
    let open Set in
        empty (assignment_of n) andalso empty ((tests_of o dag_of) n)
    end

fun unreachable(P as (ns, n0, F), n) =
    not (eqn (n0, n)) andalso Set.empty (pred(P, n))

fun read (TST(t)) = AbsMachImp.read_c t
  | read (ASS(a)) = AbsMachImp.read_o a

fun write (TST(t)) = AbsMachImp.write_c t
  | write (ASS(a)) = AbsMachImp.write_o a

fun read_write_debug (f:debug_fun) =
    if !rw_debug then print (f() ^ "\n")
    else ()

fun readNode n =
    let open Set
        val answer =
            union
            (listUnion (make::(map (read o ASS) ((set o assignment_of) n))),
             listUnion (make::(map
                               (read o TST) ((set o tests_of o dag_of) n))))
        val foo = read_write_debug
            (fn () =>
             ("readNode " ^ nodeToString n ^ "=>" ^
                 stringListString (map Int.toString (set answer))))
    in
        answer
    end

fun writeNode n =
    let open Set
        val answer =
            union
            (listUnion (make::(map (write o ASS) ((set o assignment_of) n))),
             listUnion (make::(map
                               (write o TST) ((set o tests_of o dag_of) n))))
        val foo = read_write_debug
            (fn () =>
             ("writeNode " ^ nodeToString n ^ "=>" ^
                 stringListString (map Int.toString (set answer))))
    in
        answer
    end

fun no_write_conflict (ta, n) =
    let open Set in
        empty (intersect(writeNode n, (union(read ta, write ta))))
    end

fun no_read_conflict (ta, n) =
    let open Set in
        empty (intersect (write ta, readNode n))
    end

fun empty n =
     let open Set in
         (empty o assignment_of) n andalso (empty o tests_of o dag_of) n
     end

fun replace_edge_node(N (n, a, d, p), nl) = N(n, a, replace_edge(d, nl), p)

fun except_bogus nil = nil
  | except_bogus (h::t) =
    if Delay.is_bogus_i h then except_bogus t else h :: except_bogus t

val num_ops_node = List.length o except_bogus o Set.set o assignment_of
val num_tests_node = List.length o Set.set o tests_of o dag_of
fun num_things_node n = (num_ops_node n) + (num_tests_node n)

fun dead_debug (f:debug_fun) =
    if !dead_set_debug then print ("dead" ^ f() ^ "\n") else ()

exception DEAD
fun dead(P:program, r:AbsMachImp.reg, n:node, done: name Set.set) =
    let val foo =
        dead_debug (fn () => "(P, " ^ Int.toString r ^ ", " ^ nodeToString n ^ ")")
        val new_done = Set.add(done, name_of n)
        fun nfil(a, b) = (if Set.member(new_done, a) then b else a::b)
        fun drl nil = true
          | drl (h::t) = dead(P, r, h, new_done) andalso drl t
        fun ntn n = nameToNode (P, n) handle NAMETONODE => raise DEAD
        val next = List.foldr nfil nil (Set.set (succ(P, n)))
        val answer = (
                      not (Set.member(readNode n, r)) andalso
                      (Set.member(writeNode n, r)      orelse
                       drl (map ntn next))
                      )
        val foo = dead_debug(fn () => "=>" ^ Bool.toString answer)
    in
        answer
    end

fun deadset(P, rs, n) =
    let val foo = dead_debug (fn () => "deadset(" ^
                              stringListString
                              (map Int.toString (Set.set rs)) ^ ",\n" ^
                              nodeToString n ^ ")")
        fun f nil = true
          | f (r::t) = dead(P, r, n, Set.make) andalso f t
        val answer = f (Set.set rs)
        val foo = dead_debug(fn () => "deadset=>" ^ Bool.toString answer ^ "\n")
    in
        answer
    end

fun del_debug (f:debug_fun) =
    if !delete_debug then print ("delete:" ^ f() ^ "\n")
    else ()

exception DELETE
exception DELETE_HD
exception DELETE_WIERDSUCC
fun delete (P as (ns, n0, F), n) =
    let val foo = cpsi("delete enter", P)
        val em = empty n
        val un = unreachable(P, n)
        fun ntn n = nameToNode(P, n) handle NAMETONODE => raise DELETE
        val p = Set.listToSetEQ(eqn, (map ntn (Set.set (pred(P, n)))))
        open Set

        val foo = del_debug
            (fn () =>
             "delete( n=" ^ (name_of n) ^ "\n" ^
             "em=" ^ (Bool.toString em) ^ "\n" ^
             "un=" ^ (Bool.toString un) ^ "\n" ^
             "p =" ^ (psl (map name_of (Set.set p))) ^ "\n" ^
             ")")
    in
        if (em orelse un) andalso not (eqn(n, F)) then
            if not un then
                let
                    val foo = del_debug (fn () => "complex deletion")
                    val s0 = Set.set (succ(P, n))
                    val nprime = if List.length s0 = 1 then hd s0
                                 else (print (Int.toString (List.length s0));
                                       raise DELETE_WIERDSUCC)
                    val new_nprime =
                        rmPredNode(unionPredNode(ntn nprime, pred_of n),
                                   name_of n)
                    fun ren x =
                        replace_edge_node(x, [name_of n, name_of new_nprime])
                    val pprime = map ren (set p)
                    fun updt(n, p) = updateNode(p, n)
                    val Nprime = List.foldr updt P (new_nprime :: pprime)

                    val foo = del_debug (fn () => "nprime=" ^ nprime)
                    val foo = del_debug
                        (fn () =>
                         "pprime=" ^ (psl (map nodeToString pprime)))
                    val answer = rmNode(Nprime, n)
                    val foo = cpsi("delete leave cd", answer)
                in
                    answer
                end
            else (del_debug (fn () => "simple_deletion");
                  let val s = Set.set(nameSetToNodeSet(P, (succ(P, n))))
                      fun updt(s, p) = updateNode(p, rmPredNode(s, name_of n))
                      val np = rmNode(List.foldr updt P s, n)
                      val foo = cpsi("delete leave sd", np)
                  in
                      np
                  end)
        else (del_debug (fn () => "No deletion");
              P)
    end handle Hd => raise DELETE_HD

fun mop_debug (f:debug_fun) =
    if !move_op_debug then
        (dead_set_debug := true;
         print ("mop:" ^ f() ^ "\n"))
    else dead_set_debug := false


fun can_move_op1(P as (ns, n0, F), x, move_set, m) =
    let open Set
        val foo = mop_debug (fn () => "can_move_op")
        val rok = AbsMachImp.resources_ok(set (add(assignment_of m, x)),
                                  set ((tests_of o dag_of) m))
        val foo = mop_debug(fn () => "1")
        val p = diff(nameSetToNodeSet(P, succ(P, m)), move_set)
        val foo = mop_debug(fn () => "2")
        val l = (write o ASS) x
        val foo = mop_debug(fn () => "3")
        fun dlpf nil = true
          | dlpf (pj::t) = deadset(P, l, pj) andalso dlpf t
        fun cond nil = true
          | cond (nj::t) =
            (not o eqn)(nj, F)          andalso
            (* no_read_conflict(ASS x, nj) andalso *)
            (* change ex model so it can run on a sequential machine *)
            no_read_conflict(ASS x, m) andalso
            no_write_conflict(ASS x, m) andalso
            cond t
        val foo = mop_debug(fn () => "4")
        val answer = rok andalso cond (set move_set) andalso dlpf (set p)
        val foo = mop_debug (fn () => "can_move_op=>" ^ Bool.toString answer)
    in
        answer
    end

fun can_move_op(P, x, move_set, m) =
    let open Set
        val ms = set move_set
        fun pf n = pred(P, n)
        val ps = set(listUnion (map pf ms))
    in
        if List.length ps > 1 then
            if List.length ms > 1 then false
            else List.all (fn x => can_move_op1(P, x, move_set, m)) ((set o assignment_of o hd) ms)
        else can_move_op1(P, x, move_set, m)
    end

fun move_op (P as (ns, n0, F), x, move_set, m) =
    let val foo = cpsi("move_op enter", P)
        val foo =
        mop_debug (fn () =>
                   "move_op(x=" ^
                   PrintAbs.str [x] ^
                   "move_set\n" ^
                   (stringListString (map nodeToString
                                            (Set.set move_set))) ^
                   "\nm=" ^ nodeToString m ^"\n)\n")
    in
    if not (can_move_op(P, x, move_set, m)) then P
    else
        let open Set
            exception NOTFOUND
            val primed_pairs = ref nil
            fun pnf nm =
                let fun f nil =
                    let val nn = prime_name nm
                    in
                        (primed_pairs := (nm, nn) :: !primed_pairs;
                         nn)
                    end
                      | f ((a, b)::t) = if nm = a then b else f t
                    val answer = f (!primed_pairs)
                    val foo = mop_debug (fn () => "pnf " ^ nm ^ "=>" ^ answer)
                in
                    answer
                end
            val foo = mop_debug(fn () => "1")
            fun njp nil = nil
              | njp ((N(n, a, d, prd))::t) =
                N(pnf n, rm(a, x), d, listToSet [name_of m]) :: njp t
            fun ojp l = map (fn x => rmPredNode(x, name_of m)) l
            fun replist nil = nil
              | replist (h::t) = h :: pnf h :: replist t
            val rlist = replist (map name_of (set move_set))
            val foo = mop_debug(fn () => "2")
            val mprime =
                let val aprime = add(assignment_of m, x)
                    val dprime = replace_edge(dag_of m, rlist)
                in
                    N(name_of m, aprime, dprime, pred_of m)
                end
            val foo = mop_debug(fn () => "3")
            val nj = njp(set move_set)
            val foo = mop_debug(fn () =>
                                "nj=" ^
                                stringListString (map name_of nj))
            fun uptd(n, p) = updateNode(p, n)
            val np = List.foldr uptd P (mprime :: (ojp (set move_set)))
            fun addnpi(n, p) =
                let val s = set (succNodes(p, n))
                    fun ap x = addPredNode(x, name_of n)
                    fun updt(x, p) = updateNode(p, ap x)
                in
                    List.foldr updt p s
                end
            fun addn(n, p) = addnpi(n, addNode(p, n))
            val nnp = List.foldr addn np nj
            val foo = mop_debug(fn () => "4")
            val answer = nnp
            val foo = mop_debug(fn () => "5")
            val foo = cpsi("move_op leave", answer)
        in
            mop_debug(fn () => "6");
            answer
        end
    end

fun updt_sel (d, nsel) =
    let val tst = tests_of d
        val rt = root_of d
        val s = succ_of d
    in
        newdag(tst, nsel, rt, s)
    end

fun mt_debug (f:debug_fun) =
    if !move_test_debug then print ("move_test" ^ f() ^ "\n")
    else ()

fun can_move_test(P as (ns, n0, F):program, x:test, n:node, m:node) =
    let val foo = cpsi("move_test enter", P)
        val foo = mt_debug (fn () => "can_move_test")
        val answer =
            no_write_conflict(TST x, m) andalso

            (* hack because sel can't distinguish xj *)
            not (Set.member(tests_of(dag_of m), x)) andalso

            AbsMachImp.resources_ok(Set.set (assignment_of m),
                            Set.set (Set.add((tests_of o dag_of) m, x)))
        val foo = mt_debug (fn () => "can_move_test=>" ^ Bool.toString answer)
    in
        answer
    end

fun move_test (P as (ns, n0, F):program, x:test, n:node, m:node) =
    if not (can_move_test(P, x, n, m))        then P
    else
        let val foo =
            mt_debug (fn () => "move_test" ^ name_of n ^ " " ^ name_of m)
            open Set
            val d_n = dag_of n
            val sel_n = sel_of d_n
            val rt_n = root_of d_n
            val nt =
                let val newname = (new_name o name_of) n ^ "tt"
                    fun nsel (z, b) =
                        let val v = sel_n(z, b) in
                            if toneq(v, TEST x) then sel_n(x, true)
                            else v
                        end
                    val nC =
                        if TEST x = rt_n then
                            reach(updt_sel(d_n, nsel), sel_n(x, true))
                        else
                            reach(updt_sel(d_n, nsel), rt_n)
                in
                    N(newname, assignment_of n, nC, listToSet [name_of m])
                end
            val foo = mt_debug (fn () => "got nt")
            val nf =
                let val newname = ((new_name o name_of) n) ^ "ff"
                    fun nsel (z, b) =
                        let val v = sel_n(z, b) in
                            if toneq(v, TEST x) then sel_n(x, false)
                            else v
                        end
                    val nC =
                        if TEST x = rt_n then
                            reach(updt_sel(d_n, nsel), sel_n(x, false))
                        else
                            reach(updt_sel(d_n, nsel), rt_n)
                in
                    N(newname, assignment_of n, nC, listToSet [name_of m])
                end
            val foo = mt_debug (fn () => "got nf")
            val d_m = dag_of m
            val sel_m = sel_of d_m
            fun nton n = NAME( name_of n)
            fun nsel (z, b) =
                if teq(z, x) then if b then nton nt else nton nf
                else
                    let val v = sel_m(z, b) in
                        if toneq(v, NAME(name_of n)) then TEST x else v
                    end
            val nb = add(tests_of d_m, x)
            val nh =
                add(add(rm(succ_of d_m, name_of n), name_of nt), name_of nf)
            fun new_rt (NAME rt) = TEST x
              | new_rt t = t
            val nc = newdag(nb, nsel, (new_rt o root_of) d_m, nh)
            val new_m = N(name_of m, assignment_of m, nc, pred_of m)
            fun updt_t s = addPredNode(s, name_of nt)
            fun updt_f s = addPredNode(s, name_of nf)
            val upt = map updt_t (set (nameSetToNodeSet(P, succ(P, nt))))
            val upf = map updt_f (set (nameSetToNodeSet(P, succ(P, nf))))
            fun updtl(n, p) = updateNode(p, n)
            val np =
                List.foldr updtl P ([rmPredNode(n, name_of m), new_m] @ upt @ upf)
            val answer = np
            val foo = mt_debug (fn () => "mtst done")
            val foo = cpsi("move_test leave", answer)
        in
            answer
        end


fun entries (P as (ns, n0, F)) =
    let val nl = Stringmap.extract ns
        fun f (a, b) = if unreachable(P, a) then a::b else b
    in
        n0 :: (List.foldr f nil nl)
    end

fun addPredInfo(P as (ns, n0, F)) =
    let fun rmpi n = setPredNode (n, Set.make)
        val nl = map rmpi (Stringmap.extract ns)
        fun updt(n, p) = updateNode(p, n)
        val np =  List.foldr updt P nl
        fun addpi (n, p) =
             let val s = Set.set (succNodes(p, n))
                 fun api(s, p) = updateNode(p, addPredNode(s, name_of n))
             in
                 List.foldr api p s
             end
    in
        List.foldr addpi np nl
    end

fun cp_debug (f:debug_fun) =
    if !closure_progs_debug then print ("cp:" ^ f() ^ "\n")
    else ()

fun closure (P as (ns, n0, F), entry) =
    let open Set
        val foo = cp_debug
            (fn () =>
             "closure:entry=" ^ name_of entry ^ "\nprogram=" ^ progToString P)
        val isin = Stringmap.isin
        fun dfs(p, parent, nil) = p
          | dfs(p as (ns, n0, F), parent, cur::todo) =
            if not (isin ns (name_of cur)) then
                let val np = dfs(addNode(p, cur), cur, set(succNodes(P, cur)))
                in
                    dfs(np, parent, todo)
                end
            else dfs(p, parent, todo)
        val prog:program = (Stringmap.new(), entry, F)
        val answer = dfs(addNode(prog, entry),
                         entry,
                         set(succNodes(P, entry)))
        val foo = cp_debug
            (fn () =>
             "\nclosure=>" ^ progToString answer)
    in
        answer
    end

fun programs(P as (ns, n0, F):program) =
    let val foo = cp_debug (fn () => "programs")
        val l = entries (addPredInfo P)
        (* make sure preds are in closure*)
        fun cf e = addPredInfo(closure(P, e))
        val answer = map cf l
        val foo = cp_debug (fn () => "programs done")
    in
        answer
    end

structure ns =
    struct
        type obj = node

        fun int l =
            let val z = ord #"0"
                fun f(n, nil) = n
                  | f (n, d::l) =
                    if d >= #"0" andalso d <= #"9" then f(n*10+ord(d)-z, l)
                    else n
            in
                f(0,l)
            end

        fun gt (a, b) =
            let val a = explode(name_of a)
                val b = explode(name_of b)
            in
                (int a) > (int b)
            end
    end

structure sortN = Sort(ns)

val sortNodes = sortN.sort

end
