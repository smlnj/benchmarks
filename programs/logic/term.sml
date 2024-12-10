(* term.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure Term =
struct
  datatype term
    = STR of string * term list
    | INT of int
    | CON of string
    | REF of term option ref

  exception BadArg of string
end;
