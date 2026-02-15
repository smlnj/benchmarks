(* main.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

(* NOTE: the original benchmark used the following values:
 *
 *      val grid_max = 100
 *      val step_count = 1
 *      val expectedDelta = ~33093
 *      val expected10000C = 6787);
 *)

structure Main : BMARK = Simple (
    val grid_max = 560
    val step_count = 1
    val expectedDelta = ~30673
    val expected10000C = 442);
