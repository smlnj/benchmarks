(* mi.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure MI = struct (* MONO_IDEAL *)

    (* trie:
     * index first by increasing order of vars
     * children listed in increasing degree order
     *)
    datatype 'a mono_trie = MT of 'a option * (int * 'a mono_trie) list
                            (* tag, encoded (var,pwr) and children *)
    datatype 'a mono_ideal = MI of (int * 'a mono_trie) ref
                            (* int maxDegree = least degree > all elements *)

    val op && = fn (i1, i2) => (Word.toInt (Word.andb (Word.fromInt (i1), Word.fromInt (i2))))
    val op << = fn (i1, i2) => (Word.toInt (Word.<< (Word.fromInt (i1), Word.fromInt (i2))))
    infix && <<

    fun rev ([],l) = l | rev (x::xs,l) = rev(xs,x::l)
    val emptyTrie = MT(NONE,[])
    fun mkEmpty () = MI(ref (0,emptyTrie))

    val lshift = op <<
    val andb = op &&

    fun encode (var,pwr) = lshift(var,16)+pwr
    fun grabVar vp = andb(vp,~65536)
    fun grabPwr vp = andb(vp,65535)
    fun smallerVar (vp,vp') = vp < andb(vp',~65536)

    exception Found
    fun search (MI(x),M.M m') = let
          val (d,mt) = !x
          val result = ref NONE
          (* exception Found of M.mono * '_a *)
          (* s works on remaining input mono, current output mono, tag, trie *)
          fun s (_,m,MT(SOME a,_)) =
                raise(result := SOME (M.M m,a); Found)
            | s (m',m,MT(NONE,trie)) = s'(m',m,trie)
          and s'([],_,_) = NONE
            | s'(_,_,[]) = NONE
            | s'(vp'::m',m,trie as (vp,child)::children) =
                if smallerVar(vp',vp) then s'(m',m,trie)
                else if grabPwr vp = 0 then (s(vp'::m',m,child);
                                             s'(vp'::m',m,children))
                else if smallerVar(vp,vp') then NONE
                else if vp<=vp' then (s(m',vp::m,child);
                                      s'(vp'::m',m,children))
                else NONE
          in s(rev(m',[]),[],mt)
             handle Found (* (m,a) => SOME(m,a) *) => !result
          end

   (* assume m is a new generator, i.e. not a multiple of an existing one *)
    fun insert (MI (mi),m,a) = let
          val (d,mt) = !mi
          fun i ([],MT (SOME _,_)) = Util.illegal "MONO_IDEAL.insert duplicate"
            | i ([],MT (NONE,children)) = MT(SOME a,children)
            | i (vp::m,MT(a',[])) = MT(a',[(vp,i(m,emptyTrie))])
            | i (vp::m,mt as MT(a',trie as (vp',_)::_)) = let
                fun j [] = [(vp,i(m,emptyTrie))]
                  | j ((vp',child)::children) =
                      if vp<vp' then (vp,i(m,emptyTrie))::(vp',child)::children
                      else if vp=vp' then (vp',i(m,child))::children
                      else (vp',child) :: j children
                in
                   if smallerVar(vp,vp') then
                       MT(a',[(grabVar vp,MT(NONE,trie)),(vp,i(m,emptyTrie))])
                   else if smallerVar(vp',vp) then i(grabVar vp'::vp::m,mt)
                   else MT(a',j trie)
                end
          in mi := (Int.max(d,M.deg m),i (rev(List.map encode(M.explode m),[]),mt)) end

    fun mkIdeal [] = mkEmpty()
      | mkIdeal (orig_ms : (M.mono * '_a) list)= let
          fun ins ((m,a),arr) = Util.insert((m,a),M.deg m,arr)
          val msa = Array.fromList orig_ms
          val ms : (M.mono * '_a) list =
              Util.stripSort (fn ((m,_),(m',_)) => M.compare (m,m')) msa
          val buckets = List.foldr ins (Array.array(0,[])) ms
          val n = Array.length buckets
          val mi = mkEmpty()
          fun sort i = if i>=n then mi else let
                fun redundant (m,_) = case search(mi,m) of NONE => false
                                                         | SOME _ => true
                fun filter ([],l) = List.app (fn (m,a) => insert(mi,m,a)) l
                  | filter (x::xx,l) = if redundant x then filter(xx,l)
                                       else filter(xx,x::l)
                in filter(Array.sub(buckets,i),[]);
                   Array.update(buckets,i,[]);
                   sort(i+1)
                end
          in sort 0 end

    fun fold g (MI(x)) init = let
          val (_,mt) = !x
          fun f(acc,m,MT(NONE,children)) = f'(acc,m,children)
            | f(acc,m,MT(SOME a,children)) =
                f'(g((M.M m,a),acc),m,children)
          and f'(acc,m,[]) = acc
            | f'(acc,m,(vp,child)::children) =
                if grabPwr vp=0 then f'(f(acc,m,child),m,children)
                else f'(f(acc,vp::m,child),m,children)
          in f(init,[],mt) end

end (* structure MI *)
