(* simlabs.sig
 *)

signature SIMLABS =
  sig
    exception Data_dependency_checked
    exception End_of_Program
    exception Simulator_error_1
    exception Simulator_error_2
    exception illegal_branch_within_branchdelay
    exception illegal_jump_within_branchdelay
    exception illegal_operator_or_operand
    exception negative_label_offset
    exception no_address_in_register
    exception no_label_in_register
    exception no_memory_address_in_register
    exception runtime_error_in_labwords
    exception runtime_error_in_words_or_labwords
    exception type_mismatch_in_comparison
    exception wrong_label
    val breakptr : int -> unit
    val clock : int ref
    val d_m : int * int -> unit
    val d_ms : int list -> unit
    val d_pc : unit -> unit
    val d_r : unit -> unit
    val d_regs : int list -> unit
    val init : AbsMach.opcode list -> unit
    val mcell : int -> AbsMach.values
    val pc : unit -> AbsMach.opcode list
    val pinit : int * (AbsMach.arithop -> int) * int * AbsMach.opcode list
                -> unit
    val pptr : unit -> int
    val prun : unit -> unit
    val pstep : unit -> unit
    val regc : int -> AbsMach.values
    val run : unit -> unit
    val runcount : int ref
    val step : unit -> unit
    val vinit : int * AbsMach.opcode list -> unit
    val vpc : unit -> unit
    val vrun1 : unit -> unit
    val vrun2 : unit -> unit
    val vrun3 : unit -> unit
    val vstep1 : unit -> unit
    val vstep2 : unit -> unit
    val vstep3 : unit -> unit

    val Memory : (AbsMach.values array) ref
  end
