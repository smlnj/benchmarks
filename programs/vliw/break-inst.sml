(* break-inst.sml
 *)

structure BreakInst :
    sig
        val breaki : AbsMach.opcode list -> AbsMach.opcode list
    end =
struct

open AbsMach
open AbsMachImp

val maxreg = AbsMachImp.maxreg

fun reg(i:int, s:string) = i
fun rstr(i:int, s:string) = s

val new_reg_val = ref 0
val new_reg_pairs:(AbsMach.reg * AbsMach.reg) list ref = ref nil

fun new_reg_init li = (new_reg_val := maxreg li;
                       new_reg_pairs := nil)

fun new_reg (r:AbsMach.reg) =
    let fun f nil =
        let val nr = (new_reg_val := !new_reg_val + 1; (!new_reg_val, rstr r))
        in
            (new_reg_pairs := (r, nr) :: !new_reg_pairs;
             nr)
        end
          | f ((a, b)::t) = if r = a then b else f t
    in
        f (!new_reg_pairs)
    end

fun breaki l =
    let fun f i =
        let val g =
            fn ARITH{oper, src1, src2, dst}  =>
            if reg dst = reg src1 orelse reg dst = reg src2 then
                let val nr = new_reg(dst)
                in
                    [ARITH{oper=oper, src1=src2, src2=src2, dst=nr},
                     MOVE{src=nr, dst=dst}]
                end
            else [i]
             | ARITHI{oper, src1, src2, dst}  =>
                   if reg dst = reg src1 then
                       let val nr = new_reg(dst)
                       in
                           [ARITHI{oper=oper, src1=src1, src2=src2, dst=nr},
                            MOVE{src=nr, dst=dst}]
                       end
                   else [i]
             | FETCH{immutable, offset, ptr, dst} =>
                   if reg ptr = reg dst then
                       let val nr = new_reg(dst)
                       in
                           [FETCH{immutable=immutable, offset=offset,
                                  ptr=ptr, dst=nr},
                            MOVE{src=nr, dst=dst}]
                       end
                   else [i]
             | MOVE{src, dst} =>
                   if reg src = reg dst then nil
                   else [i]
             | _ => [i]
        in
            g i
        end
        fun h (a, b) = f a @ b
        val foo = new_reg_init l
    in
        List.foldr h nil l
    end

end
