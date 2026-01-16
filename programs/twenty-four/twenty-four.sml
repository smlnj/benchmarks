(* twenty-four.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

signature TWENTY_FOUR =
  sig

    datatype expr
      = Const of int
      | Sum  of expr * expr
      | Diff of expr * expr
      | Prod of expr * expr
      | Quot of expr * expr

    val toString : expr -> string
    val solve : int list -> (expr * (unit -> 'a) -> 'a) -> (unit -> 'a) -> 'a
    val allSolutions : int list -> string list
    val firstSolution : int list -> string

  end

structure TwentyFour : TWENTY_FOUR = struct

    datatype expr
      = Const of int
      | Sum  of expr * expr
      | Diff of expr * expr
      | Prod of expr * expr
      | Quot of expr * expr

    fun eval (Const c) = Real.fromInt c
      | eval (Sum  (e1, e2)) = eval e1 + eval e2
      | eval (Diff (e1, e2)) = eval e1 - eval e2
      | eval (Prod (e1, e2)) = eval e1 * eval e2
      | eval (Quot (e1, e2)) = eval e1 / eval e2 (* div 0 will be inf *)

    fun toString exp = let
        fun lp prec opPrec = if prec > opPrec then "(" else ""
        fun rp prec opPrec = if prec > opPrec then ")" else ""
        fun s prec (Const c) = Int.toString c
          | s prec (Sum(f, g)) = concat[lp prec 0, s 0 f, " + ", s 0 g, rp prec 0]
          | s prec (Diff(f, g)) = concat[lp prec 0, s 0 f, " - ", s 1 g, rp prec 0]
          | s prec (Prod(f, g)) = concat[lp prec 2, s 2 f, " * ", s 2 g, rp prec 2]
          | s prec (Quot(f, g)) = concat[lp prec 2, s 2 f, " / ", s 3 g, rp prec 2]
        in
          s 0 exp
        end

    fun splitAt (xs, n) = let
        fun split (0, suffix, rPrefix) = (List.rev rPrefix, suffix)
          | split (_, [], _) = raise Subscript
          | split (i, x::xs, rPrefix) = split (i-1, xs, x::rPrefix)
        in
          split (n, xs, [])
        end

    (* Every (backtracking) function from here on takes two continuations --
     * succ : result * (unit -> 'a) -> 'a
     * fail : unit -> 'a
     *
     * A function calls `succ (result, resume)` to return a value and a callback
     * function to resume computation. The function calls `fail ()` when there is
     * no more computation to be done. As a result, all function calls (to
     * backtracking functions) are tail-calls.
     *)

    (* Split a list to a pair of two, non-empty lists *)
    fun partition xs succ fail = let
        val len = List.length xs
        fun p n succ fail =
            if n < len
              then succ (splitAt (xs, n), fn () => p (n + 1) succ fail)
              else fail ()
        in
          p 1 succ fail
        end

    (* Combine two expressions using all four operators *)
    fun combineExps (exp1, exp2) succ fail =
        succ (Sum  (exp1, exp2), fn () =>
        succ (Diff (exp1, exp2), fn () =>
        succ (Prod (exp1, exp2), fn () =>
        succ (Quot (exp1, exp2), fail))))

    (* Generate all possible expression trees from a list of expressions *)
    fun allTrees []  succ fail = raise Fail "impossible"
      | allTrees [x] succ fail = succ (x, fail)
      | allTrees xs  succ fail =
        partition xs
          (fn ((left, right), resume) =>
            allTrees left (fn (leftTree, resume) =>
              allTrees right (fn (rightTree, resume) =>
                combineExps (leftTree, rightTree) succ resume)
              resume)
            resume)
          fail

    (* Check if an expression evaluates to 24 *)
    fun checkExp exp succ fail =
        if Real.== (eval exp, 24.0) then succ (exp, fail) else fail ()

    (* Insert v into all positions in lst *)
    fun insert v []                 succ fail = succ ([v], fail)
      | insert v (lst as (x :: xs)) succ fail =
        succ (
          v :: lst,
          fn () => insert v xs (fn (res, resume) => succ (x :: res, resume)) fail)

    (* Generate all permutations of a list *)
    fun permutations []        succ fail = succ ([], fail)
      | permutations (x :: xs) succ fail =
        permutations xs (fn (res, resume) => insert x res succ resume) fail

    fun solve nums succ fail = let
        val nums = List.map Const nums
        in
          permutations nums
            (fn (perm, resume) =>
              allTrees perm (fn (exp, resume) => checkExp exp succ resume) resume)
            fail
        end

    fun firstSolution cards =
        solve cards (fn (sol, _) => toString sol)
                    (fn () => "no solution")

    fun allSolutions cards =
        solve cards (fn (sol, resume) => toString sol :: resume ())
                    (fn () => [])

  end
