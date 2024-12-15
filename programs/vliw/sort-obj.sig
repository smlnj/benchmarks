(* sort-obj.sig
 *)

signature SortObjSig =
    sig
        type obj
        val gt : obj * obj -> bool
    end
