(* ntypes.sml
 *)

structure Ntypes :
    sig
        type name
        val init_names : unit -> unit
        val new_name : name -> name
        val prime_name : name -> name
        val name_prefix_eq : (name * name) -> bool
        type test
        val teq : test * test -> bool
        type reg
        type assignment
        val aeq : assignment * assignment -> bool

        datatype test_or_name =
            TEST of test
          | NAME of name
          | NEITHER

        val toneq : test_or_name * test_or_name -> bool

        datatype test_or_assign =
            TST of test
          | ASS of assignment

        val toaeq : test_or_assign * test_or_assign -> bool

    end =

struct


type test = AbsMachImp.comparison
val teq = AbsMachImp.ceq

type reg = int*string

type assignment = AbsMachImp.operation
val aeq = AbsMachImp.oeq

type name = string

val ct = ref 0

fun init_names () = ct := 0

fun nn() = (ct := !ct + 1; !ct - 1)

fun pref nil = nil
  | pref (#"_" :: t) = nil
  | pref (h :: t) = h :: pref t

val name_prefix = implode o pref o explode
fun name_prefix_eq(a, b) = (name_prefix a) = (name_prefix b)
(*
fun new_name n = n ^ "_" ^ (Int.toString (nn()))
*)
fun new_name n = name_prefix n ^ "_" ^ (Int.toString (nn()))
fun prime_name n = (new_name n) ^ "'"

datatype test_or_name =
    TEST of test
  | NAME of name
  | NEITHER

fun toneq (TEST a, TEST b) = teq (a, b)
  | toneq (NAME a, NAME b) = a = b
  | toneq _ = false

datatype test_or_assign =
    TST of test
  | ASS of assignment

fun toaeq (TST a, TST b) = teq (a, b)
  | toaeq (ASS a, ASS b) = aeq (a, b)
  | toaeq _ = false

end
