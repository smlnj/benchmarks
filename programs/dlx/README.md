# DLX Simulator Benchmark

This program is a partial implementation of the RISC instruction
set described in Patterson's and Hennessy's _Computer Architecture_.
The benchmark is taken from the [MLton](http://mlton.org) sources,
where it is in the file `DLXSimulator.sml`.  The original version
was written by Matthew Fluet.  This version has been restructured
as separate source files with a CM file for the SML/NJ benchmark
repository.

# Description and History

The following is the header comment from the MLton version:

``` sml
(* More tweaks by Matthew Fluet (Matthew.Fluet@gmail.com) on 2017-12-06 to
 * generalize implementation of I/O instructions and only output the
 * last run of each DLX program in the benchmark.
 *)
(* Minor tweaks by Stephen Weeks (sweeks@sweeks.com) on 2001-07-17 to turn into a
 * benchmark.
 * Added rand function.
 *)
(*
 * Matthew Thomas Fluet
 * Harvey Mudd College
 * Claremont, CA 91711
 * e-mail: Matthew_Fluet@hmc.edu
 *
 * A DLX Simulator in Standard ML
 *
 * Description:
 * The DLX Simulator is a partial implementation of the RISC instruction
 * set described in Patterson's and Hennessy's _Computer Architecture_.
 * Currently, the DLX Simulator implements the following instructions:
 *   ADD     ADDI
 *   ADDU    ADDUI
 *   SUB     SUBI
 *   SUBU    SUBUI
 *   AND     ANDI
 *   OR      ORI
 *   XOR     XORI
 *
 *   LHI
 *
 *   SLL     SLLI
 *   SRL     SRLI
 *   SRA     SRAI
 *
 *   SEQ     SEQI
 *   SNE     SNEI
 *   SLT     SLTI
 *   SGT     SGTI
 *   SLE     SLEI
 *   SGE     SGEI
 *
 *   LB      LBU     SB
 *   LH      LHU     SH
 *   LW      SW
 *
 *   BEQZ    BNEZ
 *   J       JR
 *   JAL     JALR
 *
 *   TRAP
 *
 *   NOP
 *
 * Currently, the DLX Simulator uses 32 bit words for addressing and
 * the register file and a 65535 word memory.  To augment the memory
 * a cache can be installed in the simulator, with a number of different
 * caching options that can be made.  Caches can also cache other caches,
 * so realistic dual level caches can be simulated.  Input and output
 * is limited to requesting and outputing signed integers.
 *
 * Usage:
 * DLXSimulatorCX.run_file : string -> unit
 * DLXSimulatorCX.run_prog : string list -> unit;
 * The DLXSimualatorCX family of structures represent different caches
 * used on the simulator.  The following table describes the different
 * caches used:
 * C1: a small level 1 cache
 * DLXSimulatorCX.run_file attempts to open and execute the instructions
 * in a file.  DLXSimulatorCX.run_prog runs a set of instructions as
 * a list of strings.  Four programs are included here.
 * Simple : simply outputs the number 42.
 * Twos: performs the twos complement on an inputed number.
 * Abs: performs the absolute value on an imputed number.
 * Fact: performs the factorial on an inputed number.
 * GCD: performs the greatest common divisor on two imputed numbers.
 * After running, the DLX Simulator outputs a set of statistics
 * concerning memory reads and writes, and cache hits and misses.
 *
 * Future Work:
 * With the implementation of the PACK_REAL structures
 * as documented in the SML'97 Basis Library, the remainder
 * of the DLX instruction set should be implemented.
 * Currently, without an efficient and correct means of
 * converting a 32 bit word into a 32 bit float, it is
 * difficult to incorporate these instructions.
 * In order to finish following the current development
 * model, a FPALU structure should be implemented as the
 * floating point arithmetic-logic unit.
 * Another possibility for future work would be to
 * model a pipelined processor.  Currently, the DLX Simulator
 * uses a simple one cycle per instruction model.
 * It should be possible to break this model and implement
 * a pipeline, but it would mean a major reworking of the
 * DLXSimulatorFun functor.
 *
 * References:
 * Patterson, David A. and John L. Hennessy.  _Computer Architecture: A
 *   Quantitative Approach: Second Edition_.  San Francisco: Morgan
 *   Kaufmann Publishers, Inc., 1996.
 *
 *)
```