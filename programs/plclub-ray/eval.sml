(* eval.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

signature EVAL =
   sig
      val f : Program.t list -> unit
   end
structure Eval: EVAL =
struct

open Caml
open Program

val rtd = 180.0 / Math.acos (~1.0)
val dtr = Math.acos (~1.0) / 180.0
fun deg x = rtd * x
fun rad x = dtr * x
val zero = VFloat 0.0
val one = VFloat 1.0

fun lookup (env, s) : int =
  case env of
     [] => failwith ("Unbound variable \"" ^ s ^ "\"")
   | s' :: env' =>
        if s = s'
           then 0
        else 1 + (lookup(env', s))

(* XXX embed values *)
fun conv (absenv, p) =
   case p of
      [] => []
    | Float x :: Float y :: Float z :: Prim Point :: r =>
         Val' (VPoint (VFloat x, VFloat y, VFloat z)) :: conv(absenv, r)
    | t :: r =>
         (case t of
             Fun p' => Fun' (conv(absenv, p')) :: conv(absenv, r)
           | Arr p' => Arr' (conv(absenv, p')) :: conv(absenv, r)
           | Ident s => Ident' (lookup(absenv, s)) :: conv(absenv, r)
           | Binder s => Binder' :: conv (s :: absenv, r)
           | Int i => Val' (VInt i) :: conv(absenv, r)
           | Float f => Val' (VFloat f) :: conv(absenv, r)
           | Bool b => Val' (VBool b) :: conv(absenv, r)
           | String s => Val' (VStr s) :: conv(absenv, r)
           | Prim k => Prim' k :: conv(absenv, r))

fun inline (offset, env, p) =
   case p of
      [] => []
    | t :: r =>
         let
            fun normal() = t :: inline(offset, env, r)
         in case t of
            Fun' p' => Fun' (inline(offset, env, p')) :: inline(offset, env, r)
          | Arr' p' => Arr' (inline(offset, env, p')) :: inline(offset, env, r)
          | Ident' i =>
               if i >= offset
                  then Val' (List.nth (env, i - offset)) :: inline(offset, env, r)
               else normal()
          | Binder' => Binder' :: inline (1 + offset, env, r)
          | Prim' _ => normal()
          | Val' _ => normal()
         end

val inline_closure =
   fn (VClos (env, p)) => VClos ([], inline(0, env, p))
    | _ => failwith "a surface function was actually not a function"

val _ = Render.inline_closure := inline_closure

fun eval (env, st, p) =
  case (st, p) of
(* inlined value *)
    (_, Val' v :: r) => eval(env, (v :: st), r)
(* Rule 1 *)
(* Rule 2 *)
  | (v::st', Binder' :: r) => eval((v :: env), st', r)
(* Rule 3 *)
  | (_, Ident' i :: r) =>
      let val v = List.nth(env, i)
      in eval(env, (v :: st), r)
      end
(* Rule 4 *)
  | (_, Fun' f :: r) => eval(env, (VClos (env, f) :: st), r)
(* Rule 5 *)
  | (VClos (env', f) :: st', Prim' Apply :: r) =>
      eval(env, eval(env', st', f), r)
(* Rule 6 *)
  | (_, Arr' a :: r) =>
      eval(env, (VArr (Array.of_list (List.rev (eval(env, [], a))))) :: st, r)
(* Rules 7 and 8 *)
  | (VClos _ :: VClos (env', iftrue) :: VBool true :: st', Prim' If :: r) =>
      eval(env, eval(env', st', iftrue), r)
  | (VClos (env', iffalse) :: VClos _ :: VBool false :: st', Prim' If :: r) =>
      eval(env, eval(env', st', iffalse), r)
(* Operations on numbers *)
  | (VInt n2 :: VInt n1 :: st', Prim' Addi :: r) =>
       eval(env, (VInt (n1 + n2) :: st'), r)
  | (VFloat f2 :: VFloat f1 :: st', Prim' Addf :: r) =>
      eval(env, (VFloat (f1 + f2) :: st'), r)
  | (VFloat f :: st', Prim' Acos :: r) =>
       eval(env, (VFloat (deg (Math.acos f)) :: st'), r)
  | (VFloat f :: st', Prim' Asin :: r) =>
       eval(env, (VFloat (deg (Math.asin f)) :: st'), r)
  | ((vf as VFloat f):: st', Prim' Clampf :: r) =>
      let val f' = if f < 0.0 then zero else if f > 1.0 then one else vf
      in eval(env, (f' :: st'), r)
      end
  | (VFloat f :: st', Prim' Cos :: r) =>
       eval(env, (VFloat (Math.cos (rad f)) :: st'), r)
  | (VInt n2 :: VInt n1 :: st', Prim' Divi :: r) =>
       eval(env, (VInt (n1 div n2) :: st'), r)
  | (VFloat f2 :: VFloat f1 :: st', Prim' Divf :: r) =>
      eval(env, (VFloat (f1 / f2) :: st'), r)
  | (VInt n2 :: VInt n1 :: st', Prim' Eqi :: r) =>
       eval(env, (VBool (n1 = n2) :: st'), r)
  | (VFloat f2 :: VFloat f1 :: st', Prim' Eqf :: r) =>
      eval(env, (VBool (Real.==(f1, f2)) :: st'), r)
  | (VFloat f :: st', Prim' Floor :: r) =>
      eval(env, (VInt (Real.floor f) :: st'), r)
  | (VFloat f :: st', Prim' Frac :: r) =>
       eval(env, (VFloat (Real.realMod f) :: st'), r)
  | (VInt n2 :: VInt n1 :: st', Prim' Lessi :: r) =>
      eval(env, (VBool (n1 < n2) :: st'), r)
  | (VFloat f2 :: VFloat f1 :: st', Prim' Lessf :: r) =>
      eval(env, (VBool (f1 < f2) :: st'), r)
  | (VInt n2 :: VInt n1 :: st', Prim' Modi :: r) =>
      eval(env, (VInt (n1 mod n2) :: st'), r)
  | (VInt n2 :: VInt n1 :: st', Prim' Muli :: r) =>
       eval(env, (VInt (n1 * n2) :: st'), r)
  | (VFloat f2 :: VFloat f1 :: st', Prim' Mulf :: r) =>
      eval(env, (VFloat (f1 * f2) :: st'), r)
  | (VInt n :: st', Prim' Negi :: r) => eval(env, (VInt (~ n) :: st'), r)
  | (VFloat f :: st', Prim' Negf :: r) => eval(env, (VFloat (~ f) :: st'), r)
  | (VInt n :: st', Prim' Real :: r) => eval(env, (VFloat (real n) :: st'), r)
  | (VFloat f :: st', Prim' Sin :: r) => eval(env, (VFloat (Math.sin (rad f)) :: st'), r)
  | (VFloat f :: st', Prim' Sqrt :: r) => eval(env, (VFloat (Math.sqrt f) :: st'), r)
  | (VInt n2 :: VInt n1 :: st', Prim' Subi :: r) => eval(env, (VInt (n1 - n2) :: st'), r)
  | (VFloat f2 :: VFloat f1 :: st', Prim' Subf :: r) =>
      eval(env, (VFloat (f1 - f2) :: st'), r)
(* Operations on points *)
  | (VPoint (x, _, _) :: st', Prim' Getx :: r ) => eval(env, (x :: st'), r)
  | (VPoint (_, y, _) :: st', Prim' Gety :: r ) => eval(env, (y :: st'), r)
  | (VPoint (_, _, z) :: st', Prim' Getz :: r ) => eval(env, (z :: st'), r)
  | ((z as VFloat _) :: (y as VFloat _) :: (x as VFloat _) :: st',
     Prim' Point :: r) =>
      eval(env, (VPoint (x, y, z) :: st'), r)
  | (VInt i :: VArr a :: st', Prim' Get :: r) =>
      (* if compiled of "-unsafe" *)
      if i < 0 orelse i >= Array.length a
      then failwith "illegal access beyond array boundary"
      else eval(env, (Array.sub(a, i) :: st'), r)
  | (VArr a :: st', Prim' Length :: r) =>
      eval(env, (VInt (Array.length a) :: st'), r)
(* Geometric primitives *)
  | ((f as VClos _) :: st', Prim' Sphere :: r  ) =>
      eval(env, (VObj (OObj (OSphere, ref (Unopt f))) :: st'), r)
  | ((f as VClos _) :: st', Prim' Cube :: r    ) =>
      eval(env, (VObj (OObj (OCube, ref (Unopt f)))   :: st'), r)
  | ((f as VClos _) :: st', Prim' Cylinder :: r) =>
      eval(env, (VObj (OObj (OCylind, ref (Unopt f))) :: st'), r)
  | ((f as VClos _) :: st', Prim' Cone :: r    ) =>
      eval(env, (VObj (OObj (OCone, ref (Unopt f)))   :: st'), r)
  | ((f as VClos _) :: st', Prim' Plane :: r   ) =>
      eval(env, (VObj (OObj (OPlane, ref (Unopt f)))  :: st'), r)
(* Transformations *)
  | (VFloat z :: VFloat y :: VFloat x :: VObj ob :: st', Prim' Translate :: r) =>
      eval(env,
        (VObj (OTransform (ob,
                           Matrix.translate (x, y, z),
                           Matrix.translate (~ x, ~ y, ~ z),
                           1.0, true)) :: st'),
        r)
  | (VFloat z :: VFloat y :: VFloat x :: VObj ob :: st', Prim' Scale :: r) =>
       eval( env,
        (VObj (OTransform (ob,
                           Matrix.scale (x, y, z),
                           Matrix.unscale (x, y, z),
                           Real.max (Real.abs x,
                                     (Real.max (Real.abs y, Real.abs z))),
                           false)) :: st'),
        r)
  | (VFloat s :: VObj ob :: st', Prim' Uscale :: r) =>
      eval(env,
        (VObj (OTransform (ob, Matrix.uscale s, Matrix.unuscale s,
                           Real.abs s, true)) :: st'),
        r)
  | (VFloat t :: VObj ob :: st', Prim' Rotatex :: r) =>
      eval(env,
        (VObj (OTransform (ob, Matrix.rotatex t, Matrix.rotatex (~ t),
                           1.0, true)) :: st'),
        r)
  | (VFloat t :: VObj ob :: st', Prim' Rotatey :: r) =>
      eval(env,
        (VObj (OTransform (ob, Matrix.rotatey t, Matrix.rotatey (~ t),
                           1.0, true)) :: st'),
        r)
  | (VFloat t :: VObj ob :: st', Prim' Rotatez :: r) =>
      eval(env,
        (VObj (OTransform (ob, Matrix.rotatez t, Matrix.rotatez (~ t),
                           1.0, true)) :: st'),
        r)
(* Lights *)
  | ((color as VPoint _) :: (dir as VPoint _) :: st', Prim' Light :: r) =>
      eval(env, (VLight (dir, color) :: st'), r)
  | ((color as VPoint _) :: (pos as VPoint _) :: st', Prim' Pointlight :: r) =>
      eval(env, (VPtLight (pos, color) :: st'), r)
  | ((expon as VFloat _) :: (cutoff as VFloat _) :: (color as VPoint _) ::
    (at as VPoint _) :: (pos as VPoint _) :: st', Prim' Spotlight :: r) =>
      eval(env, (VStLight (pos, at, color, cutoff, expon) :: st'), r)
(* Constructive geometry *)
  | ((VObj o2) :: (VObj o1) :: st', Prim' Union :: r) =>
      eval(env, (VObj (OUnion (o1, o2)) :: st'), r)
  | ((VObj o2) :: (VObj o1) :: st', Prim' Intersect :: r) =>
      eval(env, (VObj (OInter (o1, o2)) :: st'), r)
  | ((VObj o2) :: (VObj o1) :: st', Prim' Difference :: r) =>
      eval(env, (VObj (ODiff (o1, o2)) :: st'), r)
(* Rendering *)
  | (VStr file :: VInt ht :: VInt wid :: VFloat fov :: VInt depth ::
    VObj obj :: VArr lights :: VPoint (VFloat ax, VFloat ay, VFloat az) ::
    st', Prim' Render :: r) =>
(*
amb the intensity of ambient light (a point).
lights is an array of lights used to illuminate the scene.
obj is the scene to render.
depth is an integer limit on the recursive depth of the ray tracing.
fov is the horizontal field of view in degrees (a real number).
wid is the width of the rendered image in pixels (an integer).
ht is the height of the rendered image in pixels (an integer).
file is a string specifying output file for the rendered image.
*)
    (Render.f ((ax, ay, az), lights, obj, depth, fov, wid, ht, file)
     ; eval(env, st', r))
(* Termination *)
  | (_, []) => st
(* Failure *)
  | _ =>
      raise (Stuck_computation (env, st, p))

fun apply (f, st) =
  case f of
    VClos (env, p) => eval(env, st, p)
  | _ => raise Fail "assert false"

val _ = Render.apply := apply

fun f p =
   let
      val st = eval([], [], (conv([], p)))
   in
      case st of
         [] => ()
       | _ => failwith "error"
   end handle Stuck_computation (env, st, p) => failwith "stuck"

end
