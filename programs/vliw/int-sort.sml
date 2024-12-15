(* int-sort.sml
 *)

structure IntImp =
    struct
        type obj = int
        fun gt(a:obj, b:obj) = a > b
    end


structure INTSort = Sort ( IntImp )
