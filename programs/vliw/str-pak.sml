(* str-pak.sml
 *)

structure StrPak :
    sig
        val stringListString : string list -> string
    end =

struct

fun sl nil = "]"
  | sl (h::nil) = h ^ "]"
  | sl (h::n::t) = h ^ "," ^ sl (n::t)

fun stringListString l = "[" ^ sl l

end
