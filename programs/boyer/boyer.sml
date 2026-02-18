(* boyer.sml:
 *
 * Tautology checker
 *)

signature BOYER =
  sig
    include TERMS
    val tautp: term -> bool
  end

structure Boyer: BOYER =
  struct

    open Terms

fun toS (Var i) = "$" ^ Int.toString i
  | toS (Prop(head, ts)) = concat[headname head, "(", String.concatWithMap "," toS ts, ")"]

    fun mem x [] = false
      | mem x (y::L) = x=y orelse mem x L

    fun truep (x, lst) = case x
         of Prop(head, _) => headname head = "true" orelse mem x lst
          | _ => mem x lst

    and falsep (x, lst) = case x
         of Prop(head, _) => headname head = "false" orelse mem x lst
          | _ => mem x lst

    fun tautologyp (x, true_lst, false_lst) =
          if truep (x, true_lst) then true else
          if falsep (x, false_lst) then false else
          (case x
           of Var _ => false
            | Prop (head,[test, yes, no]) =>
                 if headname head = "if" then
                   if truep (test, true_lst) then
                     tautologyp (yes, true_lst, false_lst)
                   else if falsep (test, false_lst) then
                     tautologyp (no, true_lst, false_lst)
                   else tautologyp (yes, test::true_lst, false_lst) andalso
                        tautologyp (no, true_lst, test::false_lst)
                 else
                   false
| _ => (print(toS x ^ "\n"); false))

    fun tautp x = tautologyp(rewrite x, [], []);

  end (* Boyer *)
