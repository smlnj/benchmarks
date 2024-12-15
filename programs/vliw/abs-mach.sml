(* abs-mach.sml
 *)

structure AbsMach =
struct
  type reg = (int*string)
  type label = (int*string)
  datatype values =
      INT of int
    | REAL of real
    | LABVAL of int * int

  datatype arithop = imul | iadd | isub | idiv
                  | orb | andb | xorb | rshift | lshift
                   | fadd | fdiv | fmul | fsub
                  | real | floor | logb

  datatype comparison = ilt | ieq | igt | ile | ige | ine
                      | flt | feq | fgt | fle | fge | fne
                      | inrange | outofrange
  datatype opcode =
      FETCH of {immutable: bool, offset: int, ptr: reg, dst: reg}
                (* dst := M[ptr+offset]
                    if immutable then unaffected by any STORE
                   other than through the allocptr *)
    | STORE of {offset: int, src: reg, ptr: reg}
                (* M[ptr+offset] := src *)
    | GETLAB of {lab: label, dst: reg}
    | GETREAL of {value: string, dst: reg}
    | ARITH of {oper: arithop, src1: reg, src2: reg, dst: reg}
    | ARITHI of {oper: arithop, src1: reg, src2: int, dst: reg}
    | MOVE of {src: reg, dst: reg}
    | BRANCH of {test: comparison, src1: reg, src2: reg, dst: label,
                 live: reg list}
    | JUMP of {dst: reg, live: reg list}
    | LABEL of {lab:label, live: reg list}
    | WORD of {value: int}
    | LABWORD of {lab: label}
    | NOP
    | BOGUS of {reads: reg list, writes: reg list}

  val opcodeEq : opcode * opcode -> bool = (op =)

end
