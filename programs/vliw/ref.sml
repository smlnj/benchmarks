(* ref.sml
 *)

structure Ref =
struct
  val inc = fn x => (x := !x + 1)
  val dec = fn x => (x := !x - 1)
end
