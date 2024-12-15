(* print-abs.sml
 *)

structure PrintAbs :
    sig
        val show: TextIO.outstream -> AbsMach.opcode list -> unit
        val str: AbsMach.opcode list -> string
    end =
struct

open AbsMach

fun xstr prog =

let

val outstr = ref ""
fun pr s = outstr := !outstr ^ s

val aop =
 fn imul => "imul"
  | iadd => "iadd"
  | isub => "isub"
  | idiv => "idiv"
  | orb => "orb"
  | andb => "andb"
  | xorb => "xorb"
  | rshift => "rshift"
  | lshift => "lshift"
  | fadd => "fadd"
  | fdiv => "fdiv"
  | fmul => "fmul"
  | fsub => "fsub"
  | real => "real"
  | floor => "floor"
  | logb => "logb"

val com =
  fn ilt => "ilt"
   | ieq => "ieq"
   | igt => "igt"
   | ile => "ile"
   | ige => "ige"
   | ine => "ine"
   | flt => "flt"
   | feq => "feq"
   | fgt => "fgt"
   | fle => "fle"
   | fge => "fge"
   | fne => "fne"
   | inrange => "inrange"
   | outofrange => "outofrange"

fun bo true = "t" | bo false = "f"

fun reg(i,s) = (pr(s); pr "/R"; pr(Int.toString i))
fun label(i,s) = (pr(s); pr "/L"; pr(Int.toString i))

val p =
  fn FETCH{immutable,offset,ptr,dst} =>
      (pr "FETCH";
       if immutable then pr "i  " else pr "m  ";
       reg dst; pr " := M[ "; reg ptr;
       pr " + "; pr (Int.toString offset); pr(" ]\n"))
   | STORE{offset,ptr,src} =>
      (pr "STORE   ";
       pr "M[ "; reg ptr;
       pr " + "; pr (Int.toString offset); pr(" ] := ");
       reg src;
       pr "\n")
   | GETLAB{lab, dst} =>
      (pr "GETLAB  "; reg dst;
       pr " := "; label lab;
       pr "\n")
   | GETREAL{value,dst} =>
      (pr "GETREAL "; reg dst;
       pr " := ";
       pr value;
       pr "\n")
   | ARITH{oper,src1,src2,dst} =>
      (pr "ARITH   "; reg dst;
       pr " := "; reg src1;
       pr " "; pr(aop oper); pr " ";
       reg src2;
       pr "\n")
   | ARITHI{oper,src1,src2,dst} =>
      (pr "ARITHI  "; reg dst;
       pr " := "; reg src1;
       pr " "; pr(aop oper); pr " ";
       pr(Int.toString src2);
       pr "\n")
   | MOVE{src,dst} =>
      (pr "MOVE    "; reg dst;
       pr " := "; reg src;
       pr "\n")
   | BRANCH{test,src1,src2,dst,live} =>
      (pr "BRANCH  ";
       pr "IF "; reg src1;
       pr " ";  pr(com test); pr " ";
       reg src2;
       pr " GOTO ";
       label dst;
       pr "   ( ";
       List.app (fn r => (reg r; pr " ")) live;
       pr ")\n")
   | JUMP{dst,live} =>
      (pr "JUMP    "; reg dst;
       pr "   ( ";
       List.app (fn r => (reg r; pr " ")) live;
       pr ")\n")
   | LABEL{lab, live} =>
      (pr "LABEL   "; label lab;
       pr ":      ( ";
       List.app (fn r => (reg r; pr " ")) live;
       pr ")\n")
   | WORD{value} =>
      (pr "WORD    ";
       pr (Int.toString value);
       pr "\n")
   | LABWORD{lab} =>
      (pr "LABWORD "; label lab;
       pr "\n")
   | NOP => pr "NOP\n"
   | BOGUS{reads, writes} =>
         (pr "BOGUS";
          pr "   ( ";
          List.app (fn r => (reg r; pr " ")) writes;
          pr ") := (";
          List.app (fn r => (reg r; pr " ")) reads;
          pr ")\n")


in (List.app p prog; !outstr)
end

fun str prog = String.concat (List.map (fn a => xstr [a]) prog)

fun show out prog =
    let fun f nil = ()
          | f (h::t) = (TextIO.output(out, xstr [h]);
                        f t)
    in
        f prog
    end

end
