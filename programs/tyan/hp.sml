(* hp.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure HP = struct
    datatype hpoly = HP of P.poly array
    val op >> = fn (i1, i2) => (Word.toInt (Word.>> (Word.fromInt (i1), Word.fromInt (i2))))
    infix >>
    val log = let
          fun log(n,l) = if n<8 then l else log((n >> 2),1+l)
          in fn n => log(n,0) end
    fun mkHPoly p = let
          val l = log(P.numTerms p)
          in HP(Array.tabulate(l+1,fn i => if i=l then p else P.zero)) end
    fun add(p,HP ps) = let
          val l = log(P.numTerms p)
          in if l>=Array.length ps then let
               val n = Array.length ps
               in HP(Array.tabulate(n+n,
                     fn i => if i<n then Array.sub(ps,i)
                             else if i=l then p else P.zero))
               end
             else let
               val p = P.add(p,Array.sub(ps,l))
               in if l=log(P.numTerms p) then (Array.update(ps,l,p); HP ps)
                  else (Array.update(ps,l,P.zero); add (p,HP ps))
               end
          end
    fun leadAndRest (HP ps) = let
          val n = Array.length ps
          fun lar (m,indices,i) = if i>=n then lar'(m,indices) else let
                val p = Array.sub(ps,i)
                in if P.isZero p then lar(m,indices,i+1)
                   else if null indices then lar(P.leadMono p,[i],i+1)
                        else case M.compare(m,P.leadMono p) of
                            LESS => lar(P.leadMono p,[i],i+1)
                          | EQUAL => lar(m,i::indices,i+1)
                          | GREATER => lar(m,indices,i+1)
                end
          and lar' (_,[]) = NONE
            | lar' (m,i::is) = let
                fun extract i = case P.leadAndRest(Array.sub(ps,i)) of
                      ((a,_),rest) => (Array.update(ps,i,rest); a)
                val a = List.foldr (fn (j,b) => F.add(extract j,b)) (extract i) is
                in if F.isZero a then lar(M.one,[],0) else SOME(a,m,HP ps)
                end
          in lar(M.one,[],0) end
  end
