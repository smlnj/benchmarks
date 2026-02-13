(* main.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure AOBench : sig

    val render : {ht : int, wid : int, nSubsamples : int} -> Word8Array.array

  end = struct

    structure W8A = Word8Array
    structure R64A = Real64Array

    val drand48 = Rand48.drand

  (** Scene parameters **)
    val naoSamples = 8

  (** for-loop combinators *)
    fun upto n f = let
          fun lp i = if (i < n) then (f i; lp(i+1)) else ()
          in
            lp 0
          end
    fun foldUpto n init f = let
          fun lp (i, acc) = if (i < n) then lp(i+1, f(i, acc)) else acc
          in
            lp (0, init)
          end

  (** Types **)
    type vec = {x : real, y : real, z : real}

    type isect = {t : real, p : vec, n : vec, hit : bool}

    type sphere = {center : vec, radius : real}

    type plane = {p : vec, n : vec}

    type ray = {org : vec, dir : vec}

  (** vector operations **)
    val vzero = {x = 0.0, y = 0.0, z = 0.0}
    fun vadd (v0 : vec, v1 : vec) = {
            x = #x v0 + #x v1, y = #y v0 + #y v1, z = #z v0 + #z v1
          }
    fun vsub (v0 : vec, v1 : vec) = {
            x = #x v0 - #x v1, y = #y v0 - #y v1, z = #z v0 - #z v1
          }
    fun vdot (v0 : vec, v1 : vec) = #x v0 * #x v1 + #y v0 * #y v1 + #z v0 * #z v1
    fun vcross (v0 : vec, v1 : vec) = {
            x = #y v0 * #z v1 - #z v0 * #y v1,
            y = #z v0 * #x v1 - #x v0 * #z v1,
            z = #x v0 * #y v1 - #y v0 * #x v1
          }
    fun vnormalize v = let
          val len = Math.sqrt (vdot (v, v))
          in
            if Real.abs len > 1.0e~17
              then {x = #x v / len, y = #y v / len, z = #z v / len}
              else v
          end

  (** ray operations **)
    fun pointAt (ray : ray, t : real) = {
            x = #x(#org ray) + t * #x(#dir ray),
            y = #y(#org ray) + t * #y(#dir ray),
            z = #z(#org ray) + t * #z(#dir ray)
          }

  (** intersection tests **)
    fun raySphereIntersect (isect : isect, ray : ray, sphere : sphere) = let
          val rs = vsub(#org ray, #center sphere)
          val b = vdot(rs, #dir ray)
          val c = vdot(rs, rs) - #radius sphere * #radius sphere
          val d = b * b - c
          in
            if (d > 0.0)
              then let
                val t = ~b - Math.sqrt d
                in
                  if (t > 0.0) andalso (t < #t isect)
                    then let
                      val pt = pointAt (ray, t)
                      in {
                        t = t, hit = true, p = pt,
                        n = vnormalize(vsub(pt, #center sphere))
                      } end
                    else isect
                end
              else isect
          end

    fun rayPlaneIntersect (isect : isect, ray : ray, plane : plane) = let
          val d = ~(vdot(#p plane, #n plane))
          val v = vdot(#dir ray, #n plane)
          in
            if Real.abs v < 1.0e~17
              then isect
              else let
                val t = ~(vdot(#org ray, #n plane) - d) / v
                in
                  if (t > 0.0) andalso (t < #t isect)
                    then {t = t, hit = true, p = pointAt(ray, t), n = #n plane}
                    else isect
                end
          end

    (* create an orthonormal basis `(vX, vY, n)` for a unit normal `n` *)
    fun orthoBasis (n : vec) = let
          val vZ = n
          val vY = if (#x n < 0.6) andalso (#x n > ~0.6)
                  then {x = 1.0, y = 0.0, z = 0.0}
                else if (#y n < 0.6) andalso (#y n > ~0.6)
                  then {x = 0.0, y = 1.0, z = 0.0}
                else if (#z n < 0.6) andalso (#z n > ~0.6)
                  then {x = 0.0, y = 0.0, z = 1.0}
                  else {x = 1.0, y = 0.0, z = 0.0}
          val vX = vnormalize (vcross (vY, vZ))
          val vY = vnormalize (vcross (vZ, vX))
          in
            (vX, vY, vZ)
          end

    val miss : isect = {t = 1.0e17, p = vzero, n = vzero, hit = false}

  (** The scene **)
    val sphere1 : sphere = { center = {x = ~2.0, y = 0.0, z = ~3.5 }, radius = 0.5 }
    val sphere2 : sphere = { center = {x = ~0.5, y = 0.0, z = ~3.0 }, radius = 0.5 }
    val sphere3 : sphere = { center = {x = 1.0, y = 0.0, z = ~2.2 }, radius = 0.5 }
    val plane : plane = {
            p = {x = 0.0, y = ~0.5, z = 0.0 },
            n = {x = 0.0, y = 1.0, z = 0.0 }
          }

  (** Rendering **)
    fun ambientOcclusion (isect : isect) = let
          val nTheta = naoSamples
          val nPhi = naoSamples
          val (vX, vY, vZ) = orthoBasis (#n isect)
          val eps = 0.0001
          val p = {
                  x = #x (#p isect) + eps * #x (#n isect),
                  y = #y (#p isect) + eps * #y (#n isect),
                  z = #z (#p isect) + eps * #z (#n isect)
                }
          val occlusion =
                foldUpto nTheta 0.0 (fn (j, occlusion) =>
                  foldUpto nPhi occlusion (fn (i, occlusion) => let
                    val theta = Math.sqrt(drand48())
                    val phi = 2.0 * Math.pi * drand48()
                    val x = Math.cos(phi) * theta
                    val y = Math.sin(phi) * theta
                    val z = Math.sqrt(1.0 - theta * theta)
                    val rx = x * #x vX + y * #x vY + z * #x vZ
                    val ry = x * #y vX + y * #y vY + z * #y vZ
                    val rz = x * #z vX + y * #z vY + z * #z vZ
                    val ray = {org = p, dir = {x = rx, y = ry, z = rz} }
                    val occIsect = raySphereIntersect(miss, ray, sphere1)
                    val occIsect = raySphereIntersect(occIsect, ray, sphere2)
                    val occIsect = raySphereIntersect(occIsect, ray, sphere3)
                    val occIsect = rayPlaneIntersect (occIsect, ray, plane)
                    in
                      if #hit occIsect then occlusion + 1.0 else occlusion
                    end))
          val nS = real(nTheta * nPhi)
          val occlusion = (nS - occlusion) / nS
          in
            { x = occlusion, y = occlusion, z = occlusion }
          end

    fun clamp f = let
          val i = Real.trunc(255.0 * f)
          in
            if (i < 0) then 0w0
            else if (i > 255) then 0w255
            else Word8.fromInt i
          end

    fun render {ht, wid, nSubsamples} = let
          val img = W8A.array(3 * ht * wid, 0w0)
          val fImg = R64A.array(3 * ht * wid, 0.0)
          fun index (row, col) = 3 * (row * wid + col)
          val nSubsamples2 = real(nSubsamples * nSubsamples)
          in
            upto ht (fn row =>
              upto wid (fn col => let
                val idx = index (row, col)
                val x = real col
                val y = real row
                in
                  upto nSubsamples (fn v =>
                    upto nSubsamples (fn u => let
                      val halfWid = real wid / 2.0
                      val px = (x + (real u / real nSubsamples) - halfWid) / halfWid
                      val halfHt = real ht / 2.0
                      val py = ~(y + (real v / real nSubsamples) - halfHt) / halfHt
                      val ray = { org = vzero, dir = vnormalize {x = px, y = py, z = ~1.0} }
                      val isect = raySphereIntersect(miss, ray, sphere1)
                      val isect = raySphereIntersect(isect, ray, sphere2)
                      val isect = raySphereIntersect(isect, ray, sphere3)
                      val isect = rayPlaneIntersect (isect, ray, plane)
                      in
                        if #hit isect
                          then let
                            val col = ambientOcclusion isect
                            in
                              R64A.update(fImg, idx+0, R64A.sub(fImg, idx+0) + #x col);
                              R64A.update(fImg, idx+1, R64A.sub(fImg, idx+1) + #y col);
                              R64A.update(fImg, idx+2, R64A.sub(fImg, idx+2) + #z col)
                            end
                          else ()
                      end));
                  R64A.update(fImg, idx+0, R64A.sub(fImg, idx+0) / nSubsamples2);
                  R64A.update(fImg, idx+1, R64A.sub(fImg, idx+1) / nSubsamples2);
                  R64A.update(fImg, idx+2, R64A.sub(fImg, idx+2) / nSubsamples2);
                  W8A.update(img, idx+0, clamp(R64A.sub(fImg, idx+0)));
                  W8A.update(img, idx+1, clamp(R64A.sub(fImg, idx+1)));
                  W8A.update(img, idx+2, clamp(R64A.sub(fImg, idx+2)))
                end));
            img
          end (* render *)

  end
