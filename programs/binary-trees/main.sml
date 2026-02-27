(* main.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure Main : BMARK =
  struct

    val name = "binary-trees"

    datatype tree = Empty | Node of tree * tree

    fun make 0 = Node(Empty, Empty)
      | make d = let val d = d - 1 in Node(make d, make d) end

    fun checksum (Node(Empty, _)) = 1
      | checksum (Node(t1, t2)) = 1 + checksum t1 + checksum t2
      | checksum _ = raise Fail "bad tree"

    fun bmark n = let
          val minDepth = 4
          val maxDepth = Int.max(n, minDepth+2)
          (* stretch tree *)
          val stretchTree = make (maxDepth+1)
          val () = Log.say [
                  "stretch tree of depth ", Int.toString(maxDepth+1),
                  "\t check: ", Int.toString(checksum stretchTree), "\n"
                ]
          (* long lived tree *)
          val longTree = make maxDepth
          fun lp1 depth = if (depth <= maxDepth)
                then let
                  val nIters = Word.toIntX(
                        Word.<<(0w1, Word.fromInt(maxDepth-depth+minDepth)))
                  fun lp2 (i, cs) = if (i <= nIters)
                        then let
                          val tr = make depth
                          in
                            lp2 (i+1, cs + checksum tr)
                          end
                        else cs
                  in
                    Log.say [
                        Int.toString nIters, "\t trees of depth ", Int.toString depth,
                        "\t check: ", Int.toString(lp2 (1, 0)), "\n"
                      ];
                    lp1 (depth + 2)
                  end
                else ()
          in
            lp1 minDepth;
            Log.say [
                "long lived tree of depth ", Int.toString maxDepth,
                "\t check: ", Int.toString(checksum longTree), "\n"
              ]
          end

    fun doit () = bmark 21

    fun testit () = bmark 10

    val results = []

  end
