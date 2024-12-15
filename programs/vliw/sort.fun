(* sort.fun
 *)

functor Sort ( objfun : SortObjSig ) :
    sig
        type obj
        val sort : obj list -> obj list
    end =

struct

open objfun

type obj = objfun.obj

fun sort l =
    let fun m2 (nil, b) = b
          | m2 (a, nil) = a
          | m2 (ha::ta, hb::tb) =
            if gt(ha, hb) then hb::(m2(ha::ta, tb))
            else ha::(m2(ta, hb::tb))
        fun ml (nil) = nil
          | ml (h::nil) = h
          | ml (h1::h2::nil) = m2(h1, h2)
          | ml (h1::h2::l) = ml [m2(h1, h2), (ml l)]
    in
        ml (map (fn x => [x]) l)
    end

end
