(* m.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure M = struct (* MONO *)
    local
        val andb = fn (i1, i2) => (Word.toInt (Word.andb (Word.fromInt (i1), Word.fromInt (i2))))
        val op << = fn (i1, i2) => (Word.toInt (Word.<< (Word.fromInt (i1), Word.fromInt (i2))))
        val op >> = fn (i1, i2) => (Word.toInt (Word.>> (Word.fromInt (i1), Word.fromInt (i2))))
        infix << >> andb
    in

(* encode (var,pwr) as a long word: hi word is var, lo word is pwr
   masks 0xffff for pwr, mask ~0x10000 for var, rshift 16 for var
   note that encoded pairs u, v have same var if u>=v, u andb ~0x10000<v
*)

    datatype mono = M of int list
    exception DoesntDivide

    val one = M []
    fun x_i v = M [(v<<16)+1]
    fun explode (M l) = List.map (fn v => (v>>16,v andb 65535)) l
    fun implode l = M (List.map (fn (v,p) => (v<<16)+p) l)

    val deg = let fun d([],n) = n | d(u::ul,n) = d(ul,(u andb 65535) + n)
              in fn (M l) => d(l,0) end

    (* x^k > y^l if x>k or x=y and k>l *)
    val compare = let
          fun cmp ([],[]) = EQUAL
            | cmp (_::_,[]) = GREATER
            | cmp ([],_::_) = LESS
            | cmp ((u::us), (v::vs)) = if u=v then cmp (us,vs)
                                  else if u<v then LESS
                                  else (* u>v *)   GREATER
          in fn (M m,M m') => cmp(m,m') end

    fun display (M (l : int list)) : string =
      let
        fun dv v = if v<26 then chr (v+ord #"a") else chr (v-26+ord #"A")
        fun d (vv,acc) = let val v = vv>>16 and p = vv andb 65535
                         in if p=1 then dv v::acc
                            else
                              (dv v)::(String.explode (Int.toString p)) @ acc
                         end
      in String.implode(List.foldl d [] l) end

    val multiply = let
          fun mul ([],m) = m
            | mul (m,[]) = m
            | mul (u::us, v::vs) = let
                val uu = u andb ~65536
                in if uu = (v andb ~65536) then let
                      val w = u + (v andb 65535)
                      in if uu = (w andb ~65536) then w::mul(us,vs)
                         else
                           (Util.illegal
                            (String.concat ["Mono.multiply overflow: ",
                                            display (M(u::us)),", ",
                                            display (M(v::vs))]))
                      end
                   else if u>v then u :: mul(us,v::vs)
                   else (* u<v *) v :: mul(u::us,vs)
                end
          in fn (M m,M m') => M (mul (m,m')) end

    val lcm = let
          fun lcm ([],m) = m
            | lcm (m,[]) = m
            | lcm (u::us, v::vs) =
                if u>=v then if (u andb ~65536)<v then u::lcm(us,vs)
                                                    else u::lcm(us,v::vs)
                        else if (v andb ~65536)<u then v::lcm(us,vs)
                                                    else v::lcm(u::us,vs)
          in fn (M m,M m') => M (lcm (m,m')) end
    val tryDivide = let
          fun rev([],l) = l | rev(x::xs,l)=rev(xs,x::l)
          fun d (m,[],q) = SOME(M(rev(q,m)))
            | d ([],_::_,_) = NONE
            | d (u::us,v::vs,q) =
                if u<v then NONE
                else if (u andb ~65536) = (v andb ~65536) then
                    if u=v then d(us,vs,q) else d(us,vs,u-(v andb 65535)::q)
                else d(us,v::vs,u::q)
          in fn (M m,M m') => d (m,m',[]) end
    fun divide (m,m') =
          case tryDivide(m,m') of SOME q => q | NONE => raise DoesntDivide

end end (* local, structure M *)
