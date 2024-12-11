(* math.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure MathExt =
struct

open Math

val epsilon = 1E~5

val dtr = acos (~1.0) / 180.0
val rtd = 180.0 / acos (~1.0)

fun dcos t = cos (t * dtr)
fun dsin t = sin (t * dtr)
fun dtan t = tan (t * dtr)
fun dacos x = rtd * acos x

end
