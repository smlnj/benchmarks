(* abs-mach-imp.sml
 *)

structure AbsMachImp :
    sig
        type reg
        type operation
        val oeq : operation * operation  -> bool
        type comparison
        val ceq : comparison * comparison -> bool
        val write_o : operation -> reg Set.set
        val read_o : operation -> reg Set.set
        val write_c : comparison -> reg Set.set
        val read_c : comparison -> reg Set.set
        val resources_ok : operation list * comparison list -> bool
        datatype codetypes =
            ASSIGNMENT of operation
          | LABELREF of int * operation
          | COMPARISON of int * operation
          | FLOW of int * operation
          | TARGET of int * operation
          | EXIT of operation
          | JUNK of operation
          | NERGLE
        val classify : operation -> codetypes
        val maxreg : AbsMach.opcode list -> int
    end =
struct

type reg = int (* register strings will gum up set operations etc *)
type operation = AbsMach.opcode
type comparison = AbsMach.opcode

fun oeq (a, b) = AbsMach.opcodeEq(a, b)
fun ceq (a, b) = AbsMach.opcodeEq(a, b)

fun reg(i, s) = i
fun label(i, s) = i


fun srl rl = Set.listToSet((map reg) rl)
fun sr r = srl [r]

val immutableMem = ~1
val mutableMem = ~2
val flowControl = ~3

(* comparisons are limited to one because of difficulty writing larger trees *)
fun resources_ok(ops, c) = (List.length ops) <= 4 andalso (List.length c) <= 1

fun allocptr r = reg r = 1

fun write_o i =
    let open Set
        open AbsMach
        val f =
            fn FETCH{dst, ...} => sr dst
             | STORE{ptr, ...} =>
                   if allocptr ptr then listToSet [immutableMem, mutableMem]
                   else listToSet [mutableMem]
             | GETLAB {dst, ...} => sr dst
             | GETREAL {dst, ...} => sr dst
             | ARITH {dst, ...} => sr dst
             | ARITHI {dst, ...} => sr dst
             | MOVE {dst, ...} => sr dst
             | JUMP _  => listToSet [flowControl]
             | BOGUS {writes, ...} => srl writes
             | _  =>  make
    in
        f i
    end

fun write_c c = Set.listToSet [flowControl]

val std_reg_list = [(1, ""), (2, ""), (3, ""), (4, ""), (5, "")]

fun read i =
    let open Set
        open AbsMach
        val f =
        fn FETCH {immutable, ptr, ...} =>
        let val mem = if immutable then immutableMem else mutableMem
        in
            add(sr ptr, mem)
        end
         | STORE {src, ptr, ...} => srl [src, ptr]
         | ARITH {src1, src2, ...} => srl [src1, src2]
         | ARITHI {src1, ...} => sr src1
         | MOVE {src, ...} => sr src
         | BRANCH {src1, src2, ...} => srl [src1, src2]
         | JUMP {dst, ...} => srl (dst :: std_reg_list)
         | BOGUS {reads, ...} => srl reads
         | _ => make
    in
        f i
    end

fun read_o i = read i
fun read_c i = read i

datatype codetypes =
    ASSIGNMENT of operation
  | LABELREF of int * operation
  | COMPARISON of int * operation
  | FLOW of int * operation
  | TARGET of int * operation
  | EXIT of operation
  | JUNK of operation
  | NERGLE

fun maxreg li =
    let fun f (a, b) = Int.max(a, b)
        val r =
            (Set.set (Set.listUnion((map write_o li) @
                                    (map read li))))
    in
        List.foldr f 0 r
    end


fun classify i =
    let open AbsMach
        val f =
        fn FETCH _ => ASSIGNMENT i
         | STORE _ => ASSIGNMENT i
         | GETLAB{lab, dst} => LABELREF(label lab, i)
         | GETREAL _  => ASSIGNMENT i
         | ARITH _  =>  ASSIGNMENT i
         | ARITHI _ =>  ASSIGNMENT i
         | MOVE{src, dst} =>
               if reg src = reg dst then NERGLE
               else ASSIGNMENT i
         | BRANCH{test,src1,src2,dst,live} =>
               if test = ieq andalso (reg src1) = (reg src2)
                   then FLOW (label dst, i)
               else COMPARISON (label dst, i)
         | JUMP _ => EXIT i
         | LABEL {lab, ...} => TARGET(label lab, i)
         | WORD _ => JUNK i
         | LABWORD _ => JUNK i
         | NOP =>  JUNK i
         | BOGUS _ =>  ASSIGNMENT i
    in
        f i
    end
end
