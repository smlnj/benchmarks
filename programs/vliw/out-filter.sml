(* out-filter.sml
 *)

structure OutFilter :
    sig
        val remnops : AbsMach.opcode list -> AbsMach.opcode list
    end =
struct

open AbsMach

fun remnops ol =
    let fun f (NOP, NOP::b) = NOP::b
          | f (a, b) = a::b
    in
        List.foldr f nil ol
    end

end
