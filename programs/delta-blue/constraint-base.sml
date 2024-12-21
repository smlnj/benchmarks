(* constraint-base.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

signature CONSTRAINT_BASE =
  sig

    type t = RepTypes.constraint

    val null : t

    val same : t * t -> bool

    val getStrength : t -> Strength.t
    val setStrength : t * Strength.t -> unit

    val isInput : t -> bool

    val execute : t -> unit
    val isSatisfied : t -> bool
    val markUnsatisfied : t -> unit
    val addToGraph : t -> unit
    val removeFromGraph : t -> unit
    val chooseMethod : t * int -> unit
    val markInputs : t * int -> unit
    val inputsKnown : t * int -> bool
    val output : t -> RepTypes.variable
    val recalculate : t -> unit
    val inputsToString : t -> string

  end

structure ConstraintBase : CONSTRAINT_BASE =
  struct

    datatype t = datatype RepTypes.constraint

    val null = let
          fun fail name _ = raise Fail name
          in
            Constraint{
                strength = ref Strength.required,
                isInput = false,
                execute = fail "execute",
                isSatisfied = fail "isSatisfied",
                markUnsatisfied = fail "markUnsatisfied",
                addToGraph = fail "addToGraph",
                removeFromGraph = fail "removeFromGraph",
                chooseMethod = fail "chooseMethod",
                markInputs = fail "markInputs",
                inputsKnown = fail "inputsKnown",
                output = fail "output",
                recalculate = fail "recalculate",
                inputsToString = fail "inputsToString"
              }
          end

    fun same (Constraint{strength=a, ...}, Constraint{strength=b, ...}) = (a = b)

    fun getStrength (Constraint{strength, ...}) = !strength
    fun setStrength (Constraint{strength, ...}, s) = strength := s

    fun isInput (Constraint{isInput, ...}) = isInput

    fun execute (c as Constraint{execute, ...}) = execute c
    fun isSatisfied (c as Constraint{isSatisfied, ...}) = isSatisfied c
    fun markUnsatisfied (c as Constraint{markUnsatisfied, ...}) = markUnsatisfied c
    fun addToGraph (c as Constraint{addToGraph, ...}) = addToGraph c
    fun removeFromGraph (c as Constraint{removeFromGraph, ...}) = removeFromGraph c
    fun chooseMethod (c as Constraint{chooseMethod, ...}, mark) = chooseMethod (c, mark)
    fun markInputs (c as Constraint{markInputs, ...}, mark) = markInputs (c, mark)
    fun inputsKnown (c as Constraint{inputsKnown, ...}, mark) = inputsKnown (c, mark)
    fun output (c as Constraint{output, ...}) = output c
    fun recalculate (c as Constraint{recalculate, ...}) = recalculate c
    fun inputsToString (c as Constraint{inputsToString, ...}) = inputsToString c

  end
