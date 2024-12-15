(* readi.sml
 *)

structure ReadI :
    sig
        val readI :
            AbsMachImp.operation list -> (AbsMachImp.operation list * Node.program list)

        val writeI :
            (AbsMachImp.operation list * Node.program list) -> AbsMachImp.operation list

        val progMap : Node.program -> string

        val read_debug : bool ref
        val write_debug : bool ref
        val live_debug : bool ref
    end =

struct

val read_debug = ref false
val write_debug = ref false
val live_debug = ref false

fun read_dbg f =
    if !read_debug then print ("readI.read:" ^ f() ^ "\n")
    else ()

fun write_dbg f =
    if !write_debug then print ("writeI.read:" ^ f() ^ "\n")
    else ()

fun write_dbg_s s = write_dbg (fn () => s)

exception BTARGET

fun btarget (nil, n) = (fn x => raise BTARGET)
  | btarget (h::t, n) =
    let open AbsMachImp
        val rf = btarget(t, n + 1)
        fun g lbl x = if lbl = x then n else rf x
        fun f (TARGET(lbl, inst)) = (g lbl)
          | f _ = rf
    in
        f h
    end


val programs = Node.programs

exception BNODES

fun buildNodes l =
    let open AbsMachImp
        open Ntypes
        val t = btarget(l, 0)
        fun f (nil, n) = nil
          | f (ci::rest, n) =
            let open Dag
                open AbsMach
                val nm = Int.toString n
                val nxtnm = Int.toString (n + 1)
                fun asn i = Set.listToSetEQ(aeq, i)
                val edag = reach(Dag.make, NAME nxtnm)
                fun tgtnm tgt = Int.toString (t tgt)
                fun edagt tgt = reach(Dag.make, NAME (tgtnm tgt))
                val finDag = reach(Dag.make, NAME (Node.name_of Node.fin))
                fun cdag (tgt,tst)  = attach(tst, edagt tgt, edag)
                val g =
                    fn ASSIGNMENT i => Node.make(nm, asn [i], edag, Set.make)
                     | NERGLE  =>  Node.make(nm, asn [], edag, Set.make)
                     | LABELREF (tgt, i as GETLAB{lab, dst}) =>
                           Node.make(nm,
                                     asn [GETLAB{lab=(t tgt, tgtnm tgt),
                                                 dst=dst}],
                                     edag, Set.make)
                     | COMPARISON (tgt, tst) =>
                           Node.make(nm, asn nil, cdag(tgt, tst), Set.make)
                     | FLOW (tgt, i) =>
                           Node.make(nm, asn nil, edagt tgt, Set.make)
                     | EXIT i => Node.make(nm, asn [i], finDag, Set.make)
                     | TARGET (lbl, i) =>
                           Node.make(nm, asn nil, edag, Set.make)
                     | _ => raise BNODES
            in
                (g ci)::Node.fin::(f (rest, n + 1))
            end
        fun addn(n, p) = Node.addNode(p, n)
        val prog = List.foldr addn (Node.makeProg()) (Node.fin :: f(l, 0))
    in
        prog
    end

exception READI
exception READI_NTN
fun readI ol =
    let open AbsMachImp
        fun junkfil (JUNK a, (junk, other)) = (JUNK a :: junk, other)
          | junkfil (x, (junk, other)) = (junk, x::other)
        val cl = map AbsMachImp.classify ol
        val (junk, other) = List.foldr junkfil (nil, nil) cl
        fun ntn x = (Node.nameToNode x )
            handle NAMETONODE => raise READI_NTN
        val (ns, foo, fin) = buildNodes other
        val nn = (ns, ntn((ns, foo, fin), "0"), fin)
        fun unjunk (JUNK i) = i
          | unjunk _ = raise READI
        val progs = programs nn
        val foo = read_dbg
            (fn () => ("progs =>" ^
                       (StrPak.stringListString
                        (map Node.progToString progs))))
    in
         (map unjunk junk,  progs)
    end

structure ps =
    struct
        open Ntypes
        type obj = Node.program

        fun int l =
            let val z = ord #"0"
                fun f(n, nil) = n
                  | f (n, d::l) =
                    if d >= #"0" andalso d <= #"9" then f(n*10+ord(d)-z, l)
                    else n
            in
                f(0,l)
            end

        fun gt((nsa, n0a, Fa), (nsb, n0b, Fb)) =
            let val a = explode (Node.name_of n0a)
                val b = explode (Node.name_of n0b)
            in
                (int a) > (int b)
            end
    end

structure sortP = Sort (ps)

fun live_dbg f = if !live_debug then print ("live:" ^ f() ^ "\n")
                 else ()

fun build_live_tab(P as (ns, n0, F): Node.program) =
    let open Ntypes
        open Node
        open Set
        fun fil (a, b) = if a < 0 orelse Delay.is_bogus_reg (a, "") then b
                         else add(b, a)
        fun fil_lset s = List.foldr fil make (set s)
        val lt:(int set) Stringmap.stringmap = Stringmap.new()
        val finset = listToSet [0, 1, 2, 3, 4, 5]
        fun flive f n =
            if Stringmap.isin lt (name_of n) then Stringmap.map lt (name_of n)
            else f n
        fun dfs cur =
            let fun fl n = flive dfs n
                val nm = name_of cur
                val gen = (fil_lset o readNode) cur
                val kill = writeNode cur
                val foo  = Stringmap.add lt (nm, gen)
                val children = succNodes(P, cur)
                val ch_live = if empty children then finset
                             else listUnion (map fl (set children))
                val live = union(diff(ch_live, kill), gen)
                val foo = Stringmap.rm lt nm
                val foo = Stringmap.add lt (nm, live)
            in
                live
            end
    in
        dfs n0;
        (fn nm =>
         let val ans = Stringmap.map lt nm
             val foo = live_dbg (fn () => nm ^ "=>" ^
                                 StrPak.stringListString
                                 (map Int.toString (set ans)))
         in
             ans
         end)
    end

(* live is the union of live in successors *)
fun branch_live (P, tab, nm) =
    let open Node
        val s = Set.set (succ(P, nameToNode(P, nm)))
        val l:int Set.set = Set.listUnion (map tab s)
        val foo = live_dbg
            (fn()=>("branch_live " ^ nm ^ " s=" ^
                    StrPak.stringListString s ^ " -> " ^
                    StrPak.stringListString (map Int.toString (Set.set l))))
    in
        l
    end

exception WRITEP
exception WRITEP1
exception WRITEP_NTN

fun writeP (entry_map,  lbl_fun, P as (ns, n0, F):Node.program) =
    let open Ntypes
        open Node
        open Set
        open AbsMachImp
        open AbsMach
        val foo = write_dbg(fn () => "program:" ^ progToString P)
        fun blblmap nil = (fn x => (print ("blblmap_" ^ x); raise WRITEP))
          | blblmap (nm::t) =
            let val mp = blblmap t
                val mylab = lbl_fun()
            in
                    (fn x => if x = nm then mylab else mp x)
            end
        val lblmap = blblmap(map name_of (Stringmap.extract ns))
        val live_tab = build_live_tab P
        fun label_list nm = map (fn r => (r, "")) (set (live_tab nm))
        fun br_list nm =
            map (fn r => (r, "")) (set (branch_live(P, live_tab, nm)))
        fun getlab (GETLAB{lab=(i,s), dst}) =
            GETLAB{lab=(entry_map s, "node" ^ s), dst=dst}
          | getlab _ = raise WRITEP1
        fun dogetlabs (i as GETLAB _, l) = (getlab i) :: l
          | dogetlabs (i, l) = i :: l
        fun ubranch (frm, nm) =
            BRANCH{test=ieq, src1=(0, "zero"), src2=(0, "zero"),
                   dst=(lblmap nm, "node" ^ nm), live=br_list frm}
        fun cbranch (BRANCH{test, src1, src2, dst, live}, frm, nm) =
            BRANCH{test=test, src1=src1, src2=src2,
                   dst=(lblmap nm, "node" ^ nm), live=br_list frm}
          | cbranch _ = (print "cbranch"; raise Match)
        fun label nm = LABEL{lab=(lblmap nm, "node" ^ nm), live=label_list nm}
        fun entry_label nm =
            LABEL{lab=(entry_map nm, "entry"), live=label_list nm}

        fun f (done, lastnm, nm) =
            let val foo = write_dbg
                    (fn () =>
                     "f (" ^
                     StrPak.stringListString (set done) ^ "," ^
                     nm ^ ")")
            in
            if nm = name_of F then (write_dbg_s "fin"; (done, [NOP]))
            else if member(done, nm) then (write_dbg_s "already";
                                           (done, [NOP, ubranch(lastnm, nm)]))
            else
                let open Dag
                    val foo = write_dbg_s "doing"
                    val node = nameToNode(P, nm)
                        handle NAMETONODE => raise WRITEP_NTN
                    val needlabel =
                        let val pd = set (pred (P, node))
                            val foo = write_dbg
                                (fn () => ("needlabel pd=" ^
                                           StrPak.stringListString pd))
                            fun f nil = false
                              | f ((p::nil):Ntypes.name list) =
                                let val pn = nameToNode(P, p:Ntypes.name)
                                    val foo = write_dbg
                                        (fn () => ("ndlbl: pn=" ^
                                                   nodeToString pn))
                                    val d = dag_of pn
                                    val sel = sel_of d
                                    val rt = root_of d
                                    fun istst (TEST t) =
                                        (write_dbg_s "ist true\n";
                                         true)
                                      | istst (NAME n) =
                                        (write_dbg_s "ist false\n";
                                         false)
                                      | istst NEITHER =
                                        (write_dbg_s "ist false\n";
                                         false)
                                    fun untst (TEST t) = t
                                      | untst _ = (print "needlabel1";
                                                  raise Match)
                                    fun unnm (NAME nm) = nm
                                      | unnm _ = (print "needlabel2";
                                                  raise Match)
                                    val foo =
                                        if istst rt then
                                            write_dbg
                                            (fn () =>
                                             ("sel=" ^
                                              unnm(sel(untst rt, true)) ^
                                              "\n"))
                                            else ()
                                in
                                    istst rt andalso
                                    (sel(untst rt, true) = NAME nm)
                                end
                              | f (a::b::c) = true
                            val answer = f pd
                            val foo = write_dbg
                                (fn () => ("needlabel=>" ^
                                           Bool.toString answer))
                        in
                            answer
                        end
                    val nodelabel = if needlabel then [label nm] else nil
                    val nodeNOP = [NOP]
                    val a = List.foldr dogetlabs nil (set (assignment_of node))
                    val d = dag_of node
                    val sel = sel_of d
                    val rt = root_of d
                    (* only works for <= 1 test *)
                    fun dag_code NEITHER = (nil, nil)
                      | dag_code (NAME n) = ([n], nil)
                      | dag_code (TEST t) =
                        let fun unnm (NAME x) = x
                              | unnm _ = (print "dag_code"; raise Match)
                            val t_n = unnm(sel(t, true))
                            val f_n = unnm(sel(t, false))
                        in
                            ([f_n, t_n], [cbranch(t, nm, t_n)])
                        end
                    val (nl, cd) = dag_code rt
                    exception DFS_SURPRISE
                    fun dfs (done, nil) = (write_dbg_s "dfs nil";
                                           (done, nil))
                      | dfs (done, h::nil) = (write_dbg_s "dfs 1";
                                              f(done, nm, h))
                      | dfs (done, h::nxt::nil) =
                        let val foo = write_dbg_s "dfs 2"
                            val (dn1, cd1) = f(done, nm, h)
                            val (dn2, cd2) =
                                if member(dn1, nxt) then (dn1, nil)
                                else dfs(dn1, nxt::nil)
                            val lbl =
                                if nxt = name_of F orelse
                                    member(dn2, nxt) then [NOP]
                                else [NOP, label nxt]
                        in
                            (dn2, cd1 @ lbl @ cd2)
                        end
                      | dfs _ = raise DFS_SURPRISE
                    val (dn, dcd) = dfs(add(done, nm), nl)
                in
                    (dn, NOP :: nodelabel @ a @ cd @ dcd)
                end
            end
            val (done, code) = f (Set.make, "badname", name_of n0)
    in
        (entry_label (name_of n0)) :: (label (name_of n0)) :: code
    end

exception WRITEI

fun progMap(p as (ns, n0, F)) =
    let val l = Node.sortNodes (Stringmap.extract ns)
        val outstr = ref ""
        fun pr s = outstr := !outstr ^ s
        fun ntn n = Node.nameToNode(p, n)
        val n0nm = Node.name_of n0
        val nFnm = Node.name_of F
        fun f n =
            let        val s = Set.set (Node.succ(p, n))
                val nm = Node.name_of n
                val pre = if nm = n0nm then "->\t"
                          else "\t"
                val post = if nm = nFnm then "\t->\n"
                           else "\n"
            in
                pr (pre ^
                    Node.name_of n ^ "\t->\t" ^ StrPak.stringListString s ^
                    post)
            end
    in
        List.app f l;
        !outstr
    end

fun writeI(j:AbsMach.opcode list, p:Node.program list) =
    let val labelid = ref 0
        fun newlabel () = (labelid := !labelid + 1; !labelid - 1)
        fun bentrymap nil = (fn x => (print ("bentrymap_" ^ x); raise WRITEI))
          | bentrymap ((ns, n0, F)::t) =
            let val mp = bentrymap t
                val mylab = newlabel()
            in
                (fn x => if x = Node.name_of n0 then mylab else mp x)
            end
        val entry_map = bentrymap p
        val sp = sortP.sort p
        fun wp p = writeP (entry_map, newlabel, p)
        fun f(a, b) = (wp a) @ b
        val i = List.foldr f nil sp
    in
        i @ j
    end


end
