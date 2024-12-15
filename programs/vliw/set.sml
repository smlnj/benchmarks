(* set.sml
 *)

structure Set :
    sig
        exception SET
        exception LISTUNION
        type 'a set
        val make : ''a set
        val makeEQ : ('a * 'a -> bool) -> 'a set
        val listToSet : ''a list -> ''a set
        val listToSetEQ : ('a * 'a -> bool) * 'a list -> 'a set
        val add : 'a set * 'a -> 'a set
        val union : 'a set * 'a set -> 'a set
        val listUnion : 'a set list -> 'a set
        val listUnionEQ : ('a * 'a -> bool) * 'a set list -> 'a set
        val rm : 'a set * 'a -> 'a set
        val intersect : 'a set * 'a set -> 'a set
        val diff : 'a set * 'a set -> 'a set
        val member : 'a set * 'a -> bool
        val set : 'a set -> 'a list
        val mag : 'a set -> int
        val empty : 'a set -> bool
    end =
struct
datatype 'a set = S of ('a*'a->bool) * 'a list

exception SET
exception LISTUNION

fun eqf (x, y) = x = y

val make = S (eqf, nil)

fun makeEQ eqf = S (eqf, nil)

fun set (S (eqf, a)) = a

fun member (S (eqf, nil), e) = false
  | member (S (eqf, (s::t)), e) = eqf(e, s) orelse member(S (eqf, t), e)

fun add(st as (S (eqf, s)), e) = if member(st, e) then st else S(eqf, e::s)

fun listToSetEQ (eqf, l) =
    let fun f (nil, s) = s
          | f (h::t, s) = f(t, add(s, h))
    in
        f(l, makeEQ eqf)
    end

fun listToSet l = listToSetEQ (eqf, l)


fun union (a, S (eqf, nil)) = a
  | union (S (eqf, nil), b) = b
  | union (S (eqf, e::a), b) = union(S (eqf, a), add(b, e))

fun listUnion (h::t) = List.foldr union h t
  | listUnion _ = raise LISTUNION

fun listUnionEQ (eqf, l) = List.foldr union (makeEQ eqf) l


fun rm (S (eqf, nil), x) = raise SET
  | rm (S (eqf, s::t), x) =
    if eqf(s, x) then S (eqf, t) else S(eqf, s :: set(rm(S (eqf, t), x)))

fun intersect1 (a, S (eqf, nil), c) = S (eqf, c)
  | intersect1 (S (eqf, nil), b, c) = S (eqf, c)
  | intersect1 (S (eqf, a::t), b, c) =
    if member(b, a) then intersect1(S (eqf, t), b, a::c)
    else intersect1(S (eqf, t), b, c)

fun intersect (a, b) = intersect1 (a, b, nil)

fun diff (S (eqf, nil), b) = S (eqf, nil)
  | diff (S (eqf, a::t), b) = if member(b, a) then diff(S (eqf, t), b)
                         else S (eqf, a :: set(diff(S (eqf, t), b)))


fun mag s = List.length (set s)

(* fun empty s = set s = nil *)

fun empty (S(eqf, nil)) = true
  | empty (S(eqf, _)) = false

end
