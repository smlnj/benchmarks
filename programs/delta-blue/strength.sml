(* strength.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure Strength :> sig

    type t

    val required : t
    val strongPreferred : t
    val preferred : t
    val strongDefault : t
    val normal : t
    val weakDefault : t
    val weakest : t

    val same : t * t -> bool

    val stronger : t * t -> bool
    val weaker : t * t -> bool
    val weakestOf : t * t -> t
    val strongest : t * t -> t

    val nextWeaker : t -> t

    val toString : t -> string

  end = struct

    datatype t = Strength of int * string

    val required = Strength(0, "required")
    val strongPreferred = Strength(1, "strongPreferred")
    val preferred = Strength(2, "preferred")
    val strongDefault = Strength(3, "strongDefault")
    val normal = Strength(4, "normal")
    val weakDefault = Strength(5, "weakDefault")
    val weakest = Strength(6, "weakest")

    fun same (Strength(s1, _), Strength(s2, _)) = s1 = s2

    fun stronger (Strength(s1, _), Strength(s2, _)) = s1 < s2
    fun weaker (Strength(s1, _), Strength(s2, _)) = s1 > s2

    fun strongest (s1, s2) = if stronger(s1, s2) then s1 else s2
    fun weakestOf (s1, s2) = if weaker(s1, s2) then s1 else s2

    fun nextWeaker (Strength(0, _)) = strongPreferred
      | nextWeaker (Strength(1, _)) = preferred
      | nextWeaker (Strength(2, _)) = strongDefault
      | nextWeaker (Strength(3, _)) = normal
      | nextWeaker (Strength(4, _)) = weakDefault
      | nextWeaker (Strength(5, _)) = weakest
      | nextWeaker _ = raise Fail "Invalid call to nextWeaker()!"

    fun toString (Strength(_, name)) = name

  end
