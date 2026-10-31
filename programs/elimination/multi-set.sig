(*#line 0.0 "basic/Multiset.sig"*)
(* ========================================================================= *)
(* A MULTISET DATATYPE FOR ML                                                *)
(* Created by Joe Hurd, July 2002                                            *)
(* ========================================================================= *)

signature Multiset =
sig

type 'a mset

val empty    : ('a * 'a -> order) -> 'a mset
val insert   : 'a * int -> 'a mset -> 'a mset
val count    : 'a mset -> 'a -> int
val union    : 'a mset -> 'a mset -> 'a mset
val compl    : 'a mset -> 'a mset
val subtract : 'a mset -> 'a mset -> 'a mset
val subset   : 'a mset -> 'a mset -> bool
val compare  : 'a mset * 'a mset -> order option
val app      : ('a * int -> unit) -> 'a mset -> unit
val to_list  : 'a mset -> ('a * int) list
val pp_mset  : 'a Useful.pp -> 'a mset Useful.pp

end
