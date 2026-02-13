(* f.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure F = struct

    val p = 17

    datatype field = F of int (* for (F n), always 0<=n<p *)

    (* exception Div = Integer.Div *)
(* unused code unless P.show, commented out in earlier version, is used
    fun show (F x) = Log.print (Int.toString x)
*)
(* unused code
    val char = p
*)

(* unused code unless P.display is used
    val zero = F 0
*)
    val one = F 1
    fun coerceInt n = F (n mod p)

    fun add (F n,F m) = let val k = n+m in if k>=p then F(k-p) else F k end
    fun subtract (F n,F m) = if n>=m then F(n-m) else F(n-m+p)
    fun negate (F 0) = F 0 | negate (F n) = F(p-n)
    fun multiply (F n,F m) = F ((n*m) mod p)
    fun reciprocal (F 0) = raise Div
      | reciprocal (F n) = let
          (* consider euclid gcd alg on (a,b) starting with a=p, b=n.
           * if maintain a = a1 n + a2 p, b = b1 n + b2 p, a>b,
           * then when 1 = a = a1 n + a2 p, have a1 = inverse of n mod p
           * note that it is not necessary to keep a2, b2 around.
           *)
          fun gcd ((a,a1),(b,b1)) =
              if b=1 then (* by continued fraction expansion, 0<|b1|<p *)
                 if b1<0 then F(p+b1) else F b1
              else let val q = a div b
                   in gcd((b,b1),(a-q*b,a1-q*b1)) end
          in gcd ((p,0),(n,1)) end
(* unused code
    fun divide (n,m) = multiply (n, reciprocal m)
*)

(* unused code unless power is used
    val andb = op &&
    val rshift = op >>
*)

(* unused code
    fun power(n,k) =
          if k<=3 then case k of
              0 => one
            | 1 => n
            | 2 => multiply(n,n)
            | 3 => multiply(n,multiply(n,n))
            | _ => reciprocal (power (n,~k)) (* know k<0 *)
          else if andb(k,1)=0 then power(multiply(n,n),rshift(k,1))
               else multiply(n,power(multiply(n,n),rshift(k,1)))
*)

    fun isZero (F n) = n=0
(* unused codeunless P.display is used
    fun equal (F n,F m) = n=m

    fun display (F n) = if n<=p div 2 then Int.toString n
                        else "-" ^ Int.toString (p-n)
*)
end
