(* read-abs.sml
 *)

structure ReadAbs : sig val read: TextIO.instream -> AbsMach.opcode list end =
struct

open AbsMach

exception ReadError

fun readline(i,f) =
let

    fun error s = (print("Error in line "^Int.toString i^": "^s^"\n");
                   raise ReadError)

fun b(#" "::rest) = b rest | b rest = rest

val aop =
 fn #"i" :: #"m" :: #"u" :: #"l"::l => (imul,l)
  | #"i" :: #"a" :: #"d" :: #"d"::l => (iadd,l)
  | #"i" :: #"s" :: #"u" :: #"b"::l => (isub,l)
  | #"i" :: #"d" :: #"i" :: #"v"::l => (idiv,l)
  | #"o" :: #"r" :: #"b" :: #" "::l=> (orb,l)
  | #"a" :: #"n" :: #"d" :: #"b"::l => (andb,l)
  | #"x" :: #"o" :: #"r" :: #"b"::l => (xorb,l)
  | #"r" :: #"s" :: #"h" :: #"i" :: #"f" :: #"t"::l => (rshift,l)
  | #"l" :: #"s" :: #"h" :: #"i" :: #"f" :: #"t"::l => (lshift,l)
  | #"f" :: #"a" :: #"d" :: #"d"::l => (fadd,l)
  | #"f" :: #"d" :: #"i" :: #"v"::l => (fdiv,l)
  | #"f" :: #"m" :: #"u" :: #"l"::l => (fmul,l)
  | #"f" :: #"s" :: #"u" :: #"b"::l => (fsub,l)
  | #"r" :: #"e" :: #"a" :: #"l"::l => (real,l)
  | #"f" :: #"l" :: #"o" :: #"o" :: #"r"::l => (floor,l)
  | #"l" :: #"o" :: #"g" :: #"b"::l => (logb,l)
  | _ => error "illegal arithmetic operator"

val com =
  fn #"i" :: #"l" :: #"t"::l => (ilt,l)
   | #"i" :: #"e" :: #"q"::l => (ieq,l)
   | #"i" :: #"g" :: #"t"::l => (igt,l)
   | #"i" :: #"l" :: #"e"::l => (ile,l)
   | #"i" :: #"g" :: #"e"::l => (ige,l)
   | #"i" :: #"n" :: #"e"::l => (ine,l)
   | #"f" :: #"l" :: #"t"::l => (flt,l)
   | #"f" :: #"e" :: #"q"::l => (feq,l)
   | #"f" :: #"g" :: #"t"::l => (fgt,l)
   | #"f" :: #"l" :: #"e"::l => (fle,l)
   | #"f" :: #"g" :: #"e"::l => (fge,l)
   | #"f" :: #"n" :: #"e"::l => (fne,l)
   | #"i" :: #"n" :: #"r" :: #"a" :: #"n" :: #"g" :: #"e"::l => (inrange,l)
   | #"o" :: #"u" :: #"t" :: #"o" :: #"f" :: #"r" :: #"a" :: #"n" :: #"g" :: #"e"::l => (outofrange,l)
   | _ => error "illegal comparison operator"

fun immut(#"i"::l) = (true,l) | immut(#"m"::l) = (false,l)
  | immut _ = error "i or m required"

fun int l =
  let val z = ord #"0"
      fun f(n,l0 as d::l) = if d >= #"0" andalso d <= #"9"
                              then f(n*10+ord(d)-z, l)
                            else (n,l0)
        | f _ = error "in readabs.int"
   in f(0,l)
  end

fun string l =
  let fun f(#"/"::l) = (nil,l)
        | f(a::l) = let val (s,l') = f l
                     in (a::s, l')
                    end
        | f _ = error "name not terminated by \"/\""
      val (s,l') = f l
   in (implode s, l')
  end

  fun realc s =
    let val (sign,s) = case explode s of #"~"::rest => (~1.0,rest)
                                       | s => (1.0,s)
        fun j(exp,d::dl,mant) = j(exp,dl,mant * 0.1 + Real.fromInt d)
          | j(0,nil,mant) = mant*sign
          | j(exp,nil,mant) = if exp>0 then j(exp-1,nil,mant*10.0)
                                       else j(exp+1,nil,mant*0.1)
        fun h(esign,wholedigits,diglist,exp,nil) =
                          j(esign*exp+wholedigits-1,diglist,0.0)
          | h(es,fd,dl,exp,d::s) = h(es,fd,dl,exp*10+(ord d - ord #"0"),s)
        fun g(i,r,#"E" :: #"~"::s)=h(~1,i,r,0,s)
          | g(i,r,#"E"::s)=h(1,i,r,0,s)
          | g(i,r,d::s) = if d >= #"0" andalso d <= #"9" then
                            g(i, (ord d - ord #"0")::r, s)
                          else h(1,i,r,0,nil)
          | g(i,r,nil) = h(1,i,r,0,nil)
        fun f(i,r,#"."::s)=g(i,r,s)
          | f(i,r,s as #"E"::_)=g(i,r,s)
          | f(i,r,d::s) = f(i+1,(ord(d)-ord(#"0"))::r,s)
          | f _ = error "bad in readdabs"
     in f(0,nil,s)
    end handle Overflow => error ("real constant "^s^" out of range")

fun require((a:char)::ar, b::br) = if a=b then require(ar,br)
                           else error(str a^" required")
  | require(nil, br) = br
  | require(a::_,_) = error(str a^" required")

fun reg l = let val (s,l) = string l
                val l = require([#"R"],l)
                val (i,l) = int l
             in ((i,s),l)
            end
fun lab l = let val (s,l) = string l
                val l = require([#"L"],l)
                val (i,l) = int l
             in ((i,s),l)
            end

fun live l =
 let fun f(#")"::_) = nil
       | f l = let val (r,l) = reg l
                in r::f(b l)
               end
  in f(b(require([#"("],l)))
 end

val opcode =
 fn #"F" :: #"E" :: #"T" :: #"C" :: #"H"::l =>
        let val (imm,l) = immut(b l)
            val (dst,l) = reg(b l)
            val (ptr,l) = reg(b(require([#"M",#"["],b(require([#":",#"="],b l)))))
            val (offset,l) = int(b(require([#"+"],b l)))
        in require([#"]"], b l);
           FETCH{immutable=imm,dst=dst,ptr=ptr,offset=offset}
        end
  | #"S" :: #"T" :: #"O" :: #"R" :: #"E"::l =>
        let val (ptr,l) = reg(b(require([#"M",#"["],b l)))
            val (offset,l) = int(b(require([#"+"],b l)))
            val (src,l) = reg(b(require([#":",#"="],b(require([#"]"], b l)))))
         in STORE{src=src,ptr=ptr,offset=offset}
        end
  | #"G" :: #"E" :: #"T" :: #"L" :: #"A" :: #"B"::l =>
        let val (dst,l) = reg(b l)
            val (lab,l) = lab(b(require([#":",#"="],b l)))
        in GETLAB{dst=dst,lab=lab}
        end
  | #"G" :: #"E" :: #"T" :: #"R" :: #"E" :: #"A" :: #"L"::l =>
        let val (dst,l) = reg(b l)
            val r = realc(implode(b(require([#":",#"="],b l))))
        in GETREAL{dst=dst,value=Real.toString r}
        end
  | #"A" :: #"R" :: #"I" :: #"T" :: #"H" :: #"I"::l =>
        let val (dst,l) = reg(b l)
            val (s1,l) = reg(b(require([#":",#"="],b l)))
            val (oper,l) = aop(b l)
            val (s2,l) = int(b l)
        in ARITHI{oper=oper,src1=s1,src2=s2,dst=dst}
        end
  | #"A" :: #"R" :: #"I" :: #"T" :: #"H"::l =>
        let val (dst,l) = reg(b l)
            val (s1,l) = reg(b(require([#":",#"="],b l)))
            val (oper,l) = aop(b l)
            val (s2,l) = reg(b l)
        in ARITH{oper=oper,src1=s1,src2=s2,dst=dst}
        end
  | #"M" :: #"O" :: #"V" :: #"E"::l =>
        let val (dst,l) = reg(b l)
            val (s1,l) = reg(b(require([#":",#"="],b l)))
        in MOVE{src=s1,dst=dst}
        end
  | #"B" :: #"R" :: #"A" :: #"N" :: #"C" :: #"H"::l =>
        let val (s1,l) = reg(b(require([#"I",#"F"],b l)))
            val (test,l) = com(b l)
            val (s2,l) = reg(b l)
            val (dst,l) = lab(b(require([#"G",#"O",#"T",#"O"],b l)))
            val liv = live(b l)
        in BRANCH{test=test,src1=s1,src2=s2,dst=dst,live=liv}
        end
  | #"J" :: #"U" :: #"M" :: #"P"::l =>
        let val (dst,l) = reg(b l)
            val live = live(b l)
         in JUMP{dst=dst,live=live}
        end
  | #"L" :: #"A" :: #"B" :: #"E" :: #"L"::l =>
        let val (lab,l) = lab(b l)
            val live = live(b(require([#":"],l)))
         in LABEL{lab=lab,live=live}
        end
  | #"W" :: #"O" :: #"R" :: #"D"::l =>
        let val (i,l) = int(b l)
         in WORD{value=i}
        end
  | #"L" :: #"A" :: #"B" :: #"W" :: #"O" :: #"R" :: #"D"::l =>
        let val (i,l) = lab(b l)
         in LABWORD{lab=i}
        end
  | #"N" :: #"O" :: #"P"::_ => NOP
  | _ => error "illegal opcode name"
in
  case TextIO.inputLine f
   of NONE => nil
    | SOME ln => opcode(b (explode ln)) :: readline(i+1,f)
end

fun read f = readline(0,f)

end
