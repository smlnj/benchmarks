(* p.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure P = struct

    datatype poly = P of (F.field*M.mono) list (* descending mono order *)
    val zero = P []
    fun coerce (a,m) = P [(a,m)]
    fun implode p = P p
    fun cons (am,P p) = P (am::p)

    val op >> = fn (i1, i2) => (Word.toInt (Word.>> (Word.fromInt (i1), Word.fromInt (i2))))
    infix >>

    val log = let fun log(n,l) = if n<=1 then l else log((n >> 1),1+l)
              in fn n => log(n,0) end
    val maxLeft = ref 0
    val maxRight = ref 0
    val counts = Array.tabulate(20,fn _ => Array.array(20,0))
    val indices = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19]

    fun pair(l,r) = let
          val l = log l and r = log r
          val _ = maxLeft := Int.max(!maxLeft,l) and _ = maxRight := Int.max(!maxRight,r)
          val a = Array.sub(counts,l)
          in Array.update(a,r,Array.sub(a,r)+1) end

local
    fun neg p = (List.map (fn (a,m) => (F.negate a,m)) p)
    fun plus ([],p2) = p2
      | plus (p1,[]) = p1
      | plus ((a,m)::ms,(b,n)::ns) = case M.compare(m,n) of
            LESS => (b,n) :: plus ((a,m)::ms,ns)
          | GREATER => (a,m) :: plus (ms,(b,n)::ns)
          | EQUAL => let val c = F.add(a,b)
                             in if F.isZero c then plus(ms,ns)
                                else (c,m)::plus(ms,ns)
                             end
    fun minus ([],p2) = neg p2
      | minus (p1,[]) = p1
      | minus ((a,m)::ms,(b,n)::ns) = case M.compare(m,n) of
            LESS => (F.negate b,n) :: minus ((a,m)::ms,ns)
          | GREATER => (a,m) :: minus (ms,(b,n)::ns)
          | EQUAL => let val c = F.subtract(a,b)
                             in if F.isZero c then minus(ms,ns)
                                else (c,m)::minus(ms,ns)
                             end
    fun termMult (a,m,p) =
          (List.map (fn (a',m') => (F.multiply(a,a'),M.multiply(m,m'))) p)
in
    fun add (P p1,P p2) = (pair(length p1,length p2); P (plus(p1,p2)))
    fun subtract (P p1,P p2) = (pair(length p1,length p2); P (minus(p1,p2)))

    fun spair (a,m,P f,b,n,P g) =
      (pair(length f,length g); P(minus(termMult(a,m,f),termMult(b,n,g))))
    val termMult = fn (a,m,P f) => P(termMult(a,m,f))
end

    fun scalarMult (a,P p) = P (List.map (fn (b,m) => (F.multiply(a,b),m)) p)

    fun isZero (P []) = true | isZero (P (_::_)) = false

    (* these should only be called if there is a leading term, i.e. poly<>0 *)
    fun leadMono (P((_,m)::_)) = m
      | leadMono (P []) = Util.illegal "POLY.leadMono"
    fun leadCoeff (P((a,_)::_)) = a
      | leadCoeff (P []) = Util.illegal "POLY.leadCoeff"
    fun rest (P (_::p)) = P p
      | rest (P []) = Util.illegal "POLY.rest"
    fun leadAndRest (P (lead::rest)) = (lead,P rest)
      | leadAndRest (P []) = Util.illegal "POLY.leadAndRest"

    fun deg (P []) = Util.illegal "POLY.deg on zero poly"
      | deg (P ((_,m)::_)) = M.deg m (* homogeneous poly *)
    fun numTerms (P p) = length p

end

