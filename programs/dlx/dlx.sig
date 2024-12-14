(* dlx.sig
 *
 * This defines the exported function provided by the DLXSimulator.
 * The function run_file takes a string corresponding to the name of the
 * file to be run, and executes it.  The function run_prog takes a
 * list of instructions and executes them.
 *)

signature DLXSIMULATOR
  = sig

      val run_file : string -> unit;
      val run_prog : {instructions: string list,
                      trap: {inputFn: {state: 'state} -> {input: int, state: 'state},
                             outputFn: {output: int, state: 'state} -> {state: 'state},
                             state: 'state}} ->
                     {state: 'state,
                      statistics: unit -> string}

    end
