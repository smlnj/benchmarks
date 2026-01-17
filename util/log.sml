(* log.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * Text output for benchmark programs.
 *)

structure Log : sig

    val withOutput : TextIO.outstream -> (unit -> unit) -> (unit -> unit)

    (* output to the current output *)
    val print : string -> unit
    val say : string list -> unit

    (* print an error message to TextIO.stdErr *)
    val error : string list -> unit

  end = struct

    val output : TextIO.outstream option ref = ref NONE

    fun withOutput outS f () = (
          output := SOME outS;
          f() handle ex => (output := NONE; raise ex);
          output := NONE)

    fun print s = (case !output
           of SOME outS => TextIO.output(outS, s)
            | NONE => ()
          (* end case *))

    fun outputList (outS, []) = ()
      | outputList (outS, s::r) = (TextIO.output(outS, s); outputList(outS, r))

    fun say msg = (case !output
           of SOME outS => outputList (outS, msg)
            | NONE => ()
          (* end case *))

    fun error msg = outputList (TextIO.stdErr, msg)

  end
