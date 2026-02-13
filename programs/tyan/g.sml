(* g.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure G = struct
    val autoReduce = ref true
    val maxDeg = ref 10000
    val maybePairs = ref 0
    val primePairs = ref 0
    val usedPairs = ref 0
    val newGens = ref 0

    val op && = fn (i1, i2) => (Word.toInt (Word.andb (Word.fromInt (i1), Word.fromInt (i2))))
    infix &&

    fun reset () = (maybePairs:=0; primePairs:=0; usedPairs:=0; newGens:=0)

    fun inc r = r := !r + 1

    fun reduce (f,mi) = if P.isZero f then f else let
          (* use accumulator and reverse at end? *)
          fun r hp = case HP.leadAndRest hp of
                NONE => []
              | (SOME(a,m,hp)) => case MI.search(mi,m) of
                    NONE => (a,m)::(r hp)
                  | SOME (m',p) => r (HP.add(P.termMult(F.negate a,M.divide(m,m'),!p),hp))
          in P.implode(r (HP.mkHPoly f)) end

    (* assume f<>0 *)
    fun mkMonic f = P.scalarMult(F.reciprocal(P.leadCoeff f),f)

    (* given monic h, a monomial ideal mi of m's tagged with g's representing
     * an ideal (g1,...,gn): a poly g is represented as (lead mono m,rest of g).
     * update pairs to include new s-pairs induced by h on g's:
     * 1) compute minimal gi1...gik so that <gij:h's> generate <gi:h's>, i.e.
     *    compute monomial ideal for gi:h's tagged with gi
     * 2) toss out gij's whose lead mono is rel. prime to h's lead mono (why?)
     * 3) put (h,gij) pairs into degree buckets: for h,gij with lead mono's m,m'
     *    deg(h,gij) = deg lcm(m,m') = deg (lcm/m) + deg m = deg (m':m) + deg m
     * 4) store list of pairs (h,g1),...,(h,gn) as vector (h,g1,...,gn)
     *)
    fun addPairs (h,mi,pairs) = let
          val m = P.leadMono h
          val d = M.deg m
          fun tag ((m' : M.mono,g' : P.poly ref),quots) = (inc maybePairs;
                                     (M.divide(M.lcm(m,m'),m),(m',!g'))::quots)
          fun insert ((mm,(m',g')),arr) = (* recall mm = m':m *)
                if M.compare(m',mm)=EQUAL then (* rel. prime *)
                    (inc primePairs; arr)
                else (inc usedPairs;
                      Util.insert(P.cons((F.one,m'),g'),M.deg mm+d,arr))
          val buckets = MI.fold insert (MI.mkIdeal (MI.fold tag mi []))
                                       (Array.array(0,[]))
          fun ins (~1,pairs) = pairs
            | ins (i,pairs) = case Array.sub(buckets,i) of
                    [] => ins(i-1,pairs)
                  | gs => ins(i-1,Util.insert(Array.fromList(h::gs),i,pairs))
          in ins(Array.length buckets - 1,pairs) end

    fun grobner fs = let
          fun pr l = Log.say (l@["\n"])
          val fs = List.foldr
                (fn (f,fs) => Util.insert(f,P.deg f,fs))
                (Array.array(0,[])) fs
          (* pairs at least as long as fs, so done when done w/ all pairs *)
          val pairs = ref(Array.array(Array.length fs,[]))
          val mi = MI.mkEmpty()
          val newDegGens = ref []
          val addGen = (* add and maybe auto-reduce new monic generator h *)
                if not(!autoReduce) then
                    fn h => MI.insert (mi,P.leadMono h,ref (P.rest h))
                else fn h => let
                    val ((_,m),rh) = P.leadAndRest h
                    fun autoReduce f =
                          if P.isZero f then f
                          else let val ((a,m'),rf) = P.leadAndRest f
                               in case M.compare(m,m') of
                                   LESS => P.cons((a,m'),autoReduce rf)
                                 | EQUAL => P.subtract(rf,P.scalarMult(a,rh))
                                 | GREATER => f
                               end
                    val rrh = ref rh
                    in
                        MI.insert (mi,P.leadMono h,rrh);
                        List.app (fn f => f:=autoReduce(!f)) (!newDegGens);
                        newDegGens := rrh :: !newDegGens
                    end
          val tasksleft = ref 0
          fun feedback () = let
                val n = !tasksleft
                in
                    if (n && 15)=0 then Log.print (Int.toString n) else ();
                        Log.print ".";
                        Log.flush();
                        tasksleft := n-1
                end

          fun try h =
              let
                  val _ = feedback ()
                  val h = reduce(h,mi)
              in if P.isZero h
                     then ()
                 else let val h = mkMonic h
                          val _ = (Log.print "#"; Log.flush())
                      in pairs := addPairs(h,mi,!pairs);
                          addGen h;
                          inc newGens
                      end
              end

          fun tryPairs fgs = let
                val ((a,m),f) = P.leadAndRest (Array.sub(fgs,0))
                fun tryPair i = if i=0 then () else let
                      val ((b,n),g) = P.leadAndRest (Array.sub(fgs,i))
                      val k = M.lcm(m,n)
                      in
                         try (P.spair(b,M.divide(k,m),f,a,M.divide(k,n),g));
                         tryPair (i-1)
                      end
                in tryPair (Array.length fgs -1) end

          fun numPairs ([],n) = n
            | numPairs (p::ps,n) = numPairs(ps,n-1+Array.length p)

          fun gb d = if d>=Array.length(!pairs) then mi else
                (* note: i nullify entries to reclaim space *)
                (
pr ["DEGREE ",Int.toString d," with ",
    Int.toString(numPairs(Array.sub(!pairs,d),0))," pairs ",
    if d>=Array.length fs then "0" else Int.toString(length(Array.sub(fs,d))),
      " generators to do"];
                 tasksleft := numPairs(Array.sub(!pairs,d),0);
                 if d>=Array.length fs then ()
                 else tasksleft := !tasksleft + length (Array.sub(fs,d));
                   if d>(!maxDeg) then ()
                   else (
                         reset();
                         newDegGens := [];
                         List.app tryPairs (Array.sub(!pairs,d));
                         Array.update(!pairs,d,[]);
                         if d>=Array.length fs then ()
                         else (List.app try (Array.sub(fs,d)); Array.update(fs,d,[]));
                           pr ["maybe ",Int.toString(!maybePairs)," prime ",
                               Int.toString (!primePairs),
                               " using ",Int.toString (!usedPairs),
                               "; found ",Int.toString (!newGens)]
                           );
                 gb(d+1)
                )
          in gb 0 end

local
    (* grammar:
     dig  ::= 0 | ... | 9
     var  ::= a | ... | z | A | ... | Z
     sign ::= + | -
     nat  ::= dig | nat dig
     mono ::=  | var mono | var num mono
     term ::= nat mono | mono
     poly ::= term | sign term | poly sign term
    *)
    datatype char = Dig of int | Var of int | Sign of int
    fun char ch =
        let val och = ord ch in
          if ord #"0"<=och andalso och<=ord #"9" then Dig (och - ord #"0")
          else if ord #"a"<=och andalso och<=ord #"z" then Var (och - ord #"a")
          else if ord #"A"<=och andalso och<=ord #"Z" then Var (och - ord #"A" + 26)
          else if och = ord #"+" then Sign 1
          else if och = ord #"-" then Sign ~1
               else Util.illegal ("bad ch in poly: " ^ (Char.toString(ch)))
        end

    fun nat (n,Dig d::l) = nat(n*10+d,l) | nat (n,l) = (n,l)
    fun mono (m,Var v::Dig d::l) =
          let val (n,l) = nat(d,l)
          in mono(M.multiply(M.implode[(v,n)],m),l) end
      | mono (m,Var v::l) = mono(M.multiply(M.x_i v,m),l)
      | mono (m,l) = (m,l)

    fun term l = let
          val (n,l) = case l of (Dig d::l) => nat(d,l) | _ => (1,l)
          val (m,l) = mono(M.one,l)
          in ((F.coerceInt n,m),l) end
    fun poly (p,[]) = p
      | poly (p,l) = let
          val (s,l) = case l of Sign s::l => (F.coerceInt s,l) | _ => (F.one,l)
          val ((a,m),l) = term l
          in poly(P.add(P.coerce(F.multiply(s,a),m),p),l) end

in
    fun parsePoly s = poly (P.zero,List.map char(String.explode s))

end (* local *)

end (* structure G *)
