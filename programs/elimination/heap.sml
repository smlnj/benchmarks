(*#line 0.0 "basic/Heap.sml"*)
(* ========================================================================= *)
(* A HEAP DATATYPE FOR ML                                                    *)
(* Created by Joe Hurd, October 2001                                         *)
(* Taken from Purely Functional Data Structures, by Chris Okasaki.           *)
(* ========================================================================= *)

(*
*)
structure Heap :> Heap =
struct

datatype 'a node = E | T of int * 'a * 'a node * 'a node;

datatype 'a heap = Heap of ('a * 'a -> order) * int * 'a node;

fun rank E = 0
  | rank (T (r, _, _, _)) = r;

fun makeT (x, a, b) =
  if rank a >= rank b then T (rank b + 1, x, a, b) else T (rank a + 1, x, b, a);

fun merge f =
  let
    fun mrg (h, E) = h
      | mrg (E, h) = h
      | mrg (h1 as T (_, x, a1, b1), h2 as T (_, y, a2, b2)) =
      (case f (x, y) of GREATER => makeT (y, a2, mrg (h1, b2))
       | _ => makeT (x, a1, mrg (b1, h2)))
  in
    mrg
  end;

fun empty f = Heap (f, 0, E);

fun add x (Heap (f, n, a)) = Heap (f, n + 1, merge f (T (1, x, E, E), a));

fun is_empty (Heap (_, _, E)) = true
  | is_empty (Heap (_, _, T _)) = false;

fun top (Heap (_, _, E)) = raise Empty
  | top (Heap (_, _, T (_, x, _, _))) = x;

fun remove (Heap (_, _, E)) = raise Empty
  | remove (Heap (f, n, T (_, x, a, b))) = (x, Heap (f, n - 1, merge f (a, b)));

fun size (Heap (_, n, _)) = n;

fun app f =
  let
    fun ap [] = ()
      | ap (E :: rest) = ap rest
      | ap (T (_, d, a, b) :: rest) = (f d; ap (a :: b :: rest))
  in
    fn Heap (_, _, a) => ap [a]
  end;

local
  fun to_lst res h =
    if is_empty h then rev res
    else let val (x, h) = remove h in to_lst (x :: res) h end;
in
  fun to_list h = to_lst [] h;
end;

local
  open Useful;
in
  fun pp_heap pp_a =
    pp_map to_list (pp_bracket ("H[", "]") (pp_sequence "," pp_a));
end;

end
