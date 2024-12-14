(* random-scene.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure RandomScene : sig

    (* `build (wid, ht, numSamples)` *)
    val build : int * int * int -> Camera.t * Object.t

  end = struct

    fun randomSphere (x, z) = let
          val chooseMat = Rand.rand()
          val c = let
                val x = real x + (0.9 * Rand.rand())
                val z = real z + (0.9 * Rand.rand())
                in
                  (x, 0.2, z)
                end
          val mat = if chooseMat < 0.8
                then Material.lambertian (
                    Rand.rand() * Rand.rand(),
                    Rand.rand() * Rand.rand(),
                    Rand.rand() * Rand.rand())
                else Material.metal (
                    ( 0.5 * (1.0 + Rand.rand()),
                      0.5 * (1.0 + Rand.rand()),
                      0.5 * (1.0 + Rand.rand()) ),
                    0.5 * Rand.rand())
          in
            Sphere.make (c, 0.2, mat)
          end

    fun makeScene () = let
          fun lp (x, z, objs) =
                if (z < 11) then lp (x, z+1, randomSphere(x, z) :: objs)
                else if (x < 11) then lp (x+1, ~11, objs)
                else objs
          in
            Object.fromList (
              lp (~11, ~11, [
                  Sphere.make((0.0, ~1000.0, 0.0), 1000.0,
                    Material.lambertian(RGB.gray 0.5)),
                  Sphere.make((4.0, 1.0, 0.0), 1.0,
                    Material.metal((0.7, 0.6, 0.5), 0.0)),
                  Sphere.make((~4.0, 1.0, 0.0), 1.0,
                    Material.lambertian(0.4, 0.2, 0.1))
                ]))
          end

    fun build (wid, ht, ns) = let
          val cam = Camera.make {
                  wid = wid, ht = ht, ns = ns,
                  pos = (13.0, 2.0, 3.0),
                  lookAt = Vec3.zero,
                  up = (0.0, 1.0, 0.0),
                  fov = 30.0
                }
          val world = makeScene()
          in
            (cam, world)
          end

  end
