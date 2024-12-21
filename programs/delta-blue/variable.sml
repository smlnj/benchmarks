(* variable.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure Variable : sig

    type t

    val new : string * int * Strength.t -> t

    val toString : t -> string

    val addConstraint : t * ConstraintBase.t -> unit
    val removeConstraint : t * ConstraintBase.t -> unit
    val getConstraints : t -> ConstraintBase.t list

    val getValue : t -> int
    val setValue : t * int -> unit

    val getDeterminedBy : t -> ConstraintBase.t
    val setDeterminedBy : t * ConstraintBase.t -> unit

    val getMark : t -> int
    val setMark : t * int -> unit

    val getWalkStrength : t -> Strength.t
    val setWalkStrength : t * Strength.t -> unit

    val getStay : t -> bool
    val setStay : t * bool -> unit

  end = struct

    datatype t = datatype RepTypes.variable

    fun new (name, v, s) = Variable{
            name = name,
            value = ref v,
            constraints = ref [],
            determinedBy = ref ConstraintBase.null,
            mark = ref 0,
            walkStrength = ref s,
            stay = ref true
          }

    fun toString (Variable{name, value, walkStrength, ...}) = concat [
            name, "(", Strength.toString(!walkStrength), ",",
            Int.toString(!value), ")"
          ]

    fun addConstraint (Variable{constraints, ...}, c) = constraints := c :: !constraints

    fun removeConstraint (Variable{constraints, determinedBy, ...}, c) = let
          fun remove ([], _) = ()
            | remove (c' :: cs, cs') = if ConstraintBase.same(c, c')
                then constraints := List.revAppend(cs', cs)
                else remove (cs, c'::cs')
          in
            remove (!constraints, []);
            if ConstraintBase.same(c, !determinedBy)
              then determinedBy := ConstraintBase.null
              else ()
          end

    fun getConstraints (Variable{constraints, ...}) = !constraints

    fun getValue (Variable{value, ...}) = !value
    fun setValue (Variable{value, ...}, v) = value := v

    fun getDeterminedBy (Variable{determinedBy, ...}) = !determinedBy
    fun setDeterminedBy (Variable{determinedBy, ...}, c) = determinedBy := c

    fun getMark (Variable{mark, ...}) = !mark
    fun setMark (Variable{mark, ...}, m) = mark := m

    fun getWalkStrength (Variable{walkStrength, ...}) = !walkStrength
    fun setWalkStrength (Variable{walkStrength, ...}, s) = walkStrength := s

    fun getStay (Variable{stay, ...}) = !stay
    fun setStay (Variable{stay, ...}, b) = stay := b

  end
