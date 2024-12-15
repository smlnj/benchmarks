(* delay.sml
 *)

structure Delay  :
    sig
        val init: AbsMach.opcode list -> unit
        val add_delay: AbsMach.opcode list ->  AbsMach.opcode list
        val rm_bogus: AbsMach.opcode list -> AbsMach.opcode list
        val is_bogus_i : AbsMach.opcode -> bool
        val is_bogus_reg : AbsMach.reg -> bool
        val idempotency : int ref
    end =
struct

open AbsMach

val maxreg = ref 0
val maxdelay = 12

val idempotency = ref 0

fun is_bogus_i (BOGUS _ ) = true
  | is_bogus_i _ = false

fun bogus_reg ((i, s), which) = (!maxreg + maxdelay * i + which, s)

fun is_bogus_reg (i, s) = i > !maxreg

fun unbogus_reg (i, s) = if is_bogus_reg (i, s) then (i div maxdelay, s)
                         else (i, s)

val max_bog_reg = ref 0
val curr_idem_reg = ref 0

fun idem_reg() =
    (curr_idem_reg := !curr_idem_reg + 1;
     (!curr_idem_reg, "idem"))

fun init il = (
               maxreg := AbsMachImp.maxreg il;
               max_bog_reg := (!maxreg + 1) *  maxdelay;
               curr_idem_reg := !max_bog_reg + 1
               )

exception DELAY

fun delay i =
    let fun opdelay oper =
        let val f =
            fn imul => 5
             | iadd => 2
             | isub => 2
             | idiv => 12
             | orb => 2
             | andb => 2
             | xorb => 2
             | rshift => 2
             | lshift => 2
             | fadd => 2
             | fdiv => 12
             | fmul => 4
             | fsub => 2
             | real => 2
             | floor => 2
             | logb => 2
        in
            f oper
        end
        val id =
            fn FETCH{immutable,offset,ptr,dst} => 2
             | STORE{offset,ptr,src} => 2
             | GETLAB{lab, dst} => 2
             | GETREAL{value,dst} => 2
             | ARITH{oper,src1,src2,dst} => opdelay oper
             | ARITHI{oper,src1,src2,dst} => opdelay oper
             | MOVE{src,dst} => 1
             | BRANCH{test,src1,src2,dst,live} => 5
             | JUMP{dst,live} => 1
             | LABEL{lab, live} => 0
             | NOP => 1
             | _ => raise DELAY
    in
        id i
    end

fun b_idemx (0, r, w) = nil
  | b_idemx (1, r, w) = BOGUS{reads=r @ w, writes = [idem_reg()]} :: nil
  | b_idemx (n, r, w) =
    let val ir = idem_reg()
    in
        BOGUS{reads=r @ w, writes = [ir]} :: b_idemx(n-1, r, [ir])
    end

fun b_idem (n, r, w) =
    let fun fil ((i, s), b) = if i = 0 then b else (i, s) :: b
        val nr = List.foldr fil nil r
    in
        if null nr then nil
        else b_idemx(n, nr, w)
    end

fun b_assx (0, r) = nil
  | b_assx (1, r) = BOGUS{reads=[bogus_reg(r, 1)], writes=[r]} :: nil
  | b_assx (n, r) =
    BOGUS{reads=[bogus_reg(r, n)], writes=[bogus_reg(r, n-1)]} ::
    b_assx(n-1, r)

fun b_ass(n, r) = BOGUS{reads=[r], writes=[bogus_reg(r, n-1)]} ::
    b_assx(n-1, r)

fun b_brxx (0, rl) = nil
  | b_brxx (1, rl) =
    let fun b r = bogus_reg(r, 1)
    in
        BOGUS{reads=rl, writes=map b rl} :: nil
    end
  | b_brxx (n, rl) =
    let fun br r = bogus_reg(r, n - 1)
        fun bw r = bogus_reg(r, n)
    in
        BOGUS{reads=map br rl, writes=map bw rl} :: b_brxx (n - 1, rl)
    end

fun b_brx (n, rl) =
    let fun br r = bogus_reg(r, n-1)
    in
        BOGUS{reads=map br rl, writes=rl} :: b_brxx(n-1, rl)
    end

fun b_br (b, n, rl) = rev (b :: b_brx(n, rl))

fun is_flow i =
    let open AbsMachImp
        fun f (FLOW _) = true
          | f _ = false
    in
        f (classify i)
    end

fun add_delay il =
    let fun idem (r, w) = b_idem (!idempotency, r, w)
        fun g i =
        let val d = delay i
            val f =
                fn FETCH{immutable,offset,ptr,dst} =>
                i :: (idem([ptr], [dst]) @ b_ass(d, dst))
                 | STORE{offset,ptr,src} => [i]
                 | GETLAB{lab, dst} => i :: b_ass(d, dst)
                 | GETREAL{value,dst} => i :: b_ass(d, dst)
                 | ARITH{oper,src1,src2,dst} =>
                       i :: (idem([src1, src2], [dst]) @ b_ass(d, dst))
                 | ARITHI{oper,src1,src2,dst} =>
                       i :: (idem([src1], [dst]) @ b_ass(d, dst))
                 | MOVE{src,dst} => i :: idem([src], [dst])
                 | BRANCH{test,src1,src2,dst,live} =>
                       if is_flow i then [i]
                       else
                           b_br (BRANCH{test=test,
                                        src1=src1,src2=src2,dst=dst,
                                        live=live},
                                 d, [src1, src2])
                 | _  =>  [i]
        in
            f i
        end
        fun apnd (nil, b) = b
          | apnd (a::t, b) = a :: apnd(t, b)
        fun fld(a, b) = apnd(g a, b)
    in
        List.foldr fld nil il
    end

fun rm_bogus il =
    let fun g nil = nil
          | g (i::t) =
        let val f =
            fn FETCH{immutable,offset,ptr,dst} =>
            FETCH{immutable=immutable, offset=offset, ptr=ptr,
                  dst= unbogus_reg dst} ::
            g t
             | STORE{offset,ptr,src} => i :: g t
             | GETLAB{lab, dst} =>
                   GETLAB{lab=lab, dst= unbogus_reg dst} :: g t
             | GETREAL{value,dst} =>
                   GETREAL{value=value, dst=unbogus_reg dst} :: g t
             | ARITH{oper,src1,src2,dst} =>
                   ARITH{oper=oper,src1=src1,src2=src2,dst=unbogus_reg dst} ::
                   g t
             | ARITHI{oper,src1,src2,dst} =>
                   ARITHI{oper=oper,src1=src1,src2=src2,dst=unbogus_reg dst} ::
                   g t
             | MOVE{src,dst} => i :: g t
             | BRANCH{test,src1,src2,dst,live} =>
                   BRANCH{test=test,
                          src1=unbogus_reg src1,
                          src2=unbogus_reg src2,
                          dst=dst, live=live
                          } :: g t
             | BOGUS _ => g t
             | _  =>  i :: g t
        in
            f i
        end
    in
        g il
    end
end
