(* main.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure Examples : sig

    (* The Standard example
     *
     * In direct-style:
     *
     * (let* ((id (lambda (x) x))
     *        (a  (id (lambda (z) (halt z))))
     *        (b  (id (lambda (y) (halt y)))))
     *   (halt b))
     *)
    val stdExample : CPS.call

    (* multiplication distributes over addition test (Church encoding):
     *
     *  (2 * (1 + 3)) = ((2 * 1) + (2 * 3)
     *)
    val bigExample : CPS.call

  end = struct

    (* label generation *)
    local
      val labelCount = ref 1
    in
    fun reset () = (labelCount := 1)
    fun newLabel () = let val n = !labelCount + 1 in labelCount := n; n end
    end (* local *)

    fun var x = CPS.VAR(Var.new x)
    fun lam formals call = CPS.LAMBDA(newLabel(), List.map Var.new formals, call)
    fun call f args = CPS.CALL(newLabel(), f, args)
    fun app f x = CPS.CALL(newLabel(), f, [x])
    fun app2 f x y = CPS.CALL(newLabel(), f, [x, y])
    fun halt x = CPS.HALT(newLabel(), Var.new x)
    fun mkLet x e1 e2 = call (lam [x] e2) [e1]

    val stdExample = (
          reset();
          mkLet "id" (lam ["x", "k"] (call (var "k") [var "x"]))
            (call (var "id") [
                (lam ["z"] (halt "z")),
                (lam ["a"]
                  (call (var "id") [
                      (lam ["y"] (halt "y")),
                      (lam ["b"] (halt "b"))
                    ]))
              ]))

    val bigExample = (
          reset();
          (app
           (lam ["$f43"]
                (app
                 (lam ["$e44"] (app (var "$f43") (var "$e44")))
                 (lam ["p1", "$k45"]
                      (app
                       (var "$k45")
                       (lam ["p2", "$k46"]
                            (app
                             (var "$k46")
                             (lam ["pf", "$k47"]
                                  (app
                                   (var "$k47")
                                   (lam ["x", "$k48"]
                                        (app
                                         (lam ["$f55"]
                                              (app
                                               (lam ["$e56"]
                                                    (app2
                                                     (var "$f55")
                                                     (var "$e56")
                                                     (lam ["$f49"]
                                                          (app
                                                           (lam ["$f53"]
                                                                (app
                                                                 (lam ["$e54"]
                                                                      (app2
                                                                       (var "$f53")
                                                                       (var "$e54")
                                                                       (lam ["$f51"]
                                                                            (app
                                                                             (lam ["$e52"]
                                                                                  (app2
                                                                                   (var "$f51")
                                                                                   (var "$e52")
                                                                                   (lam ["$e50"] (app2 (var "$f49") (var "$e50") (var "$k48")))))
                                                                             (var "x")))))
                                                                 (var "pf")))
                                                           (var "p2")))))
                                               (var "pf")))
                                         (var "p1")))))))))))
           (lam ["plus", "$k57"]
                (app
                 (lam ["$f58"]
                      (app
                       (lam ["$e59"] (app2 (var "$f58") (var "$e59") (var "$k57")))
                       (lam ["m1", "$k60"]
                            (app
                             (var "$k60")
                             (lam ["m2", "$k61"]
                                  (app
                                   (var "$k61")
                                   (lam ["mf", "$k62"]
                                        (app
                                         (lam ["$f63"]
                                              (app
                                               (lam ["$f65"]
                                                    (app
                                                     (lam ["$e66"]
                                                          (app2
                                                           (var "$f65")
                                                           (var "$e66")
                                                           (lam ["$e64"] (app2 (var "$f63") (var "$e64") (var "$k62")))))
                                                     (var "mf")))
                                               (var "m1")))
                                         (var "m2")))))))))
                 (lam ["mult", "$k67"]
                      (app
                       (lam ["$f68"]
                            (app
                             (lam ["$e69"] (app2 (var "$f68") (var "$e69") (var "$k67")))
                             (lam ["n", "$k70"]
                                  (app
                                   (var "$k70")
                                   (lam ["rf", "$k71"]
                                        (app
                                         (var "$k71")
                                         (lam ["rx", "$k72"]
                                              (app
                                               (lam ["$f79"]
                                                    (app
                                                     (lam ["$e80"]
                                                          (app2
                                                           (var "$f79")
                                                           (var "$e80")
                                                           (lam ["$f76"]
                                                                (app
                                                                 (lam ["$e77"]
                                                                      (app2
                                                                       (var "$f76")
                                                                       (var "$e77")
                                                                       (lam ["$f73"]
                                                                            (app
                                                                             (lam ["$e74"] (app2 (var "$f73") (var "$e74") (var "$k72")))
                                                                             (lam ["id", "$k75"] (app (var "$k75") (var "id")))))))
                                                                 (lam ["ignored", "$k78"] (app (var "$k78") (var "rx")))))))
                                                     (lam ["g", "$k81"]
                                                          (app
                                                           (var "$k81")
                                                           (lam ["h", "$k82"]
                                                                (app
                                                                 (lam ["$f83"]
                                                                      (app
                                                                       (lam ["$f85"]
                                                                            (app
                                                                             (lam ["$e86"]
                                                                                  (app2
                                                                                   (var "$f85")
                                                                                   (var "$e86")
                                                                                   (lam ["$e84"] (app2 (var "$f83") (var "$e84") (var "$k82")))))
                                                                             (var "rf")))
                                                                       (var "g")))
                                                                 (var "h")))))))
                                               (var "n")))))))))
                       (lam ["pred", "$k87"]
                            (app
                             (lam ["$f88"]
                                  (app
                                   (lam ["$e89"] (app2 (var "$f88") (var "$e89") (var "$k87")))
                                   (lam ["s1", "$k90"]
                                        (app
                                         (var "$k90")
                                         (lam ["s2", "$k91"]
                                              (app
                                               (lam ["$f94"]
                                                    (app
                                                     (lam ["$e95"]
                                                          (app2
                                                           (var "$f94")
                                                           (var "$e95")
                                                           (lam ["$f92"]
                                                                (app
                                                                 (lam ["$e93"] (app2 (var "$f92") (var "$e93") (var "$k91")))
                                                                 (var "s1")))))
                                                     (var "pred")))
                                               (var "s2")))))))
                             (lam ["sub", "$k96"]
                                  (app
                                   (lam ["$f97"]
                                        (app
                                         (lam ["$e98"] (app2 (var "$f97") (var "$e98") (var "$k96")))
                                         (lam ["f0", "$k99"]
                                              (app
                                               (var "$k99")
                                               (lam ["x0", "$k100"] (app (var "$k100") (var "x0")))))))
                                   (lam ["church0", "$k101"]
                                        (app
                                         (lam ["$f102"]
                                              (app
                                               (lam ["$e103"] (app2 (var "$f102") (var "$e103") (var "$k101")))
                                               (lam ["f1", "$k104"]
                                                    (app
                                                     (var "$k104")
                                                     (lam ["x1", "$k105"]
                                                          (app
                                                           (lam ["$f106"]
                                                                (app
                                                                 (lam ["$e107"] (app2 (var "$f106") (var "$e107") (var "$k105")))
                                                                 (var "x1")))
                                                           (var "f1")))))))
                                         (lam ["church1", "$k108"]
                                              (app
                                               (lam ["$f109"]
                                                    (app
                                                     (lam ["$e110"] (app2 (var "$f109") (var "$e110") (var "$k108")))
                                                     (lam ["f2", "$k111"]
                                                          (app
                                                           (var "$k111")
                                                           (lam ["x2", "$k112"]
                                                                (app
                                                                 (lam ["$f113"]
                                                                      (app
                                                                       (lam ["$f115"]
                                                                            (app
                                                                             (lam ["$e116"]
                                                                                  (app2
                                                                                   (var "$f115")
                                                                                   (var "$e116")
                                                                                   (lam ["$e114"] (app2 (var "$f113") (var "$e114") (var "$k112")))))
                                                                             (var "x2")))
                                                                       (var "f2")))
                                                                 (var "f2")))))))
                                               (lam ["church2", "$k117"]
                                                    (app
                                                     (lam ["$f118"]
                                                          (app
                                                           (lam ["$e119"] (app2 (var "$f118") (var "$e119") (var "$k117")))
                                                           (lam ["f3", "$k120"]
                                                                (app
                                                                 (var "$k120")
                                                                 (lam ["x3", "$k121"]
                                                                      (app
                                                                       (lam ["$f122"]
                                                                            (app
                                                                             (lam ["$f124"]
                                                                                  (app
                                                                                   (lam ["$f126"]
                                                                                        (app
                                                                                         (lam ["$e127"]
                                                                                              (app2
                                                                                               (var "$f126")
                                                                                               (var "$e127")
                                                                                               (lam ["$e125"]
                                                                                                    (app2
                                                                                                     (var "$f124")
                                                                                                     (var "$e125")
                                                                                                     (lam ["$e123"] (app2 (var "$f122") (var "$e123") (var "$k121")))))))
                                                                                         (var "x3")))
                                                                                   (var "f3")))
                                                                             (var "f3")))
                                                                       (var "f3")))))))
                                                     (lam ["church3", "$k128"]
                                                          (app
                                                           (lam ["$f129"]
                                                                (app
                                                                 (lam ["$e130"] (app2 (var "$f129") (var "$e130") (var "$k128")))
                                                                 (lam ["ta", "$k131"]
                                                                      (app
                                                                       (var "$k131")
                                                                       (lam ["tb", "$k132"]
                                                                            (app
                                                                             (lam ["$f133"]
                                                                                  (app
                                                                                   (lam ["$e134"] (app2 (var "$f133") (var "$e134") (var "$k132")))
                                                                                   (lam ["adummy", "$k135"] (app (var "$k135") (var "adummy")))))
                                                                             (var "ta")))))))
                                                           (lam ["true", "$k136"]
                                                                (app
                                                                 (lam ["$f137"]
                                                                      (app
                                                                       (lam ["$e138"] (app2 (var "$f137") (var "$e138") (var "$k136")))
                                                                       (lam ["fa", "$k139"]
                                                                            (app
                                                                             (var "$k139")
                                                                             (lam ["fb", "$k140"]
                                                                                  (app
                                                                                   (lam ["$f141"]
                                                                                        (app
                                                                                         (lam ["$e142"] (app2 (var "$f141") (var "$e142") (var "$k140")))
                                                                                         (lam ["bdummy", "$k143"] (app (var "$k143") (var "bdummy")))))
                                                                                   (var "fb")))))))
                                                                 (lam ["false", "$k144"]
                                                                      (app
                                                                       (lam ["$f145"]
                                                                            (app
                                                                             (lam ["$e146"] (app2 (var "$f145") (var "$e146") (var "$k144")))
                                                                             (lam ["z", "$k147"]
                                                                                  (app
                                                                                   (lam ["$f150"]
                                                                                        (app
                                                                                         (lam ["$e151"]
                                                                                              (app2
                                                                                               (var "$f150")
                                                                                               (var "$e151")
                                                                                               (lam ["$f148"]
                                                                                                    (app
                                                                                                     (lam ["$e149"] (app2 (var "$f148") (var "$e149") (var "$k147")))
                                                                                                     (var "true")))))
                                                                                         (lam ["zx", "$k152"] (app (var "$k152") (var "false")))))
                                                                                   (var "z")))))
                                                                       (lam ["church0?", "$k153"]
                                                                            (app
                                                                             (lam ["$f154"]
                                                                                  (app
                                                                                   (lam ["$e155"] (app2 (var "$f154") (var "$e155") (var "$k153")))
                                                                                   (lam ["yf", "$k156"]
                                                                                        (app
                                                                                         (lam ["$f157"]
                                                                                              (app
                                                                                               (lam ["$e158"] (app2 (var "$f157") (var "$e158") (var "$k156")))
                                                                                               (lam ["yx", "$k159"]
                                                                                                    (app
                                                                                                     (lam ["$f160"]
                                                                                                          (app
                                                                                                           (lam ["$e161"] (app2 (var "$f160") (var "$e161") (var "$k159")))
                                                                                                           (lam ["yv", "$k162"]
                                                                                                                (app
                                                                                                                 (lam ["$f165"]
                                                                                                                      (app
                                                                                                                       (lam ["$e166"]
                                                                                                                            (app2
                                                                                                                             (var "$f165")
                                                                                                                             (var "$e166")
                                                                                                                             (lam ["$f163"]
                                                                                                                                  (app
                                                                                                                                   (lam ["$e164"]
                                                                                                                                        (app2 (var "$f163") (var "$e164") (var "$k162")))
                                                                                                                                   (var "yv")))))
                                                                                                                       (var "yx")))
                                                                                                                 (var "yx")))))
                                                                                                     (var "yf")))))
                                                                                         (lam ["yg", "$k167"]
                                                                                              (app
                                                                                               (lam ["$f168"]
                                                                                                    (app
                                                                                                     (lam ["$e169"] (app2 (var "$f168") (var "$e169") (var "$k167")))
                                                                                                     (var "yg")))
                                                                                               (var "yg")))))))
                                                                             (lam ["Y", "$k170"]
                                                                                  (app
                                                                                   (lam ["$f171"]
                                                                                        (app
                                                                                         (lam ["$f173"]
                                                                                              (app
                                                                                               (lam ["$e174"]
                                                                                                    (app2
                                                                                                     (var "$f173")
                                                                                                     (var "$e174")
                                                                                                     (lam ["$e172"] (app2 (var "$f171") (var "$e172") (var "$k170")))))
                                                                                               (lam ["church=?", "$k175"]
                                                                                                    (app
                                                                                                     (var "$k175")
                                                                                                     (lam ["e1", "$k176"]
                                                                                                          (app
                                                                                                           (var "$k176")
                                                                                                           (lam ["e2", "$k177"]
                                                                                                                (app
                                                                                                                 (lam ["$f205"]
                                                                                                                      (app
                                                                                                                       (lam ["$e206"]
                                                                                                                            (app2
                                                                                                                             (var "$f205")
                                                                                                                             (var "$e206")
                                                                                                                             (lam ["$f200"]
                                                                                                                                  (app
                                                                                                                                   (lam ["$e201"]
                                                                                                                                        (app2
                                                                                                                                         (var "$f200")
                                                                                                                                         (var "$e201")
                                                                                                                                         (lam ["$f178"]
                                                                                                                                              (app
                                                                                                                                               (lam ["$e179"] (app2 (var "$f178") (var "$e179") (var "$k177")))
                                                                                                                                               (lam ["elsedummy1", "$k180"]
                                                                                                                                                    (app
                                                                                                                                                     (lam ["$f198"]
                                                                                                                                                          (app
                                                                                                                                                           (lam ["$e199"]
                                                                                                                                                                (app2
                                                                                                                                                                 (var "$f198")
                                                                                                                                                                 (var "$e199")
                                                                                                                                                                 (lam ["$f196"]
                                                                                                                                                                      (app
                                                                                                                                                                       (lam ["$e197"]
                                                                                                                                                                            (app2
                                                                                                                                                                             (var "$f196")
                                                                                                                                                                             (var "$e197")
                                                                                                                                                                             (lam ["$f181"]
                                                                                                                                                                                  (app
                                                                                                                                                                                   (lam ["$e182"] (app2 (var "$f181") (var "$e182") (var "$k180")))
                                                                                                                                                                                   (lam ["elsedummy2", "$k183"]
                                                                                                                                                                                        (app
                                                                                                                                                                                         (lam ["$f190"]
                                                                                                                                                                                              (app
                                                                                                                                                                                               (lam ["$f194"]
                                                                                                                                                                                                    (app
                                                                                                                                                                                                     (lam ["$e195"]
                                                                                                                                                                                                          (app2
                                                                                                                                                                                                           (var "$f194")
                                                                                                                                                                                                           (var "$e195")
                                                                                                                                                                                                           (lam ["$f192"]
                                                                                                                                                                                                                (app
                                                                                                                                                                                                                 (lam ["$e193"]
                                                                                                                                                                                                                      (app2
                                                                                                                                                                                                                       (var "$f192")
                                                                                                                                                                                                                       (var "$e193")
                                                                                                                                                                                                                       (lam ["$e191"]
                                                                                                                                                                                                                            (app2
                                                                                                                                                                                                                             (var "$f190")
                                                                                                                                                                                                                             (var "$e191")
                                                                                                                                                                                                                             (lam ["$f184"]
                                                                                                                                                                                                                                  (app
                                                                                                                                                                                                                                   (lam ["$f188"]
                                                                                                                                                                                                                                        (app
                                                                                                                                                                                                                                         (lam ["$e189"]
                                                                                                                                                                                                                                              (app2
                                                                                                                                                                                                                                               (var "$f188")
                                                                                                                                                                                                                                               (var "$e189")
                                                                                                                                                                                                                                               (lam ["$f186"]
                                                                                                                                                                                                                                                    (app
                                                                                                                                                                                                                                                     (lam ["$e187"]
                                                                                                                                                                                                                                                          (app2
                                                                                                                                                                                                                                                           (var "$f186")
                                                                                                                                                                                                                                                           (var "$e187")
                                                                                                                                                                                                                                                           (lam ["$e185"] (app2 (var "$f184") (var "$e185") (var "$k183")))))
                                                                                                                                                                                                                                                     (var "church1")))))
                                                                                                                                                                                                                                         (var "e2")))
                                                                                                                                                                                                                                   (var "sub")))))))
                                                                                                                                                                                                                 (var "church1")))))
                                                                                                                                                                                                     (var "e1")))
                                                                                                                                                                                               (var "sub")))
                                                                                                                                                                                         (var "church=?")))))))
                                                                                                                                                                       (var "false")))))
                                                                                                                                                           (var "e2")))
                                                                                                                                                     (var "church0?")))))))
                                                                                                                                   (lam ["thendummy", "$k202"]
                                                                                                                                        (app
                                                                                                                                         (lam ["$f203"]
                                                                                                                                              (app
                                                                                                                                               (lam ["$e204"] (app2 (var "$f203") (var "$e204") (var "$k202")))
                                                                                                                                               (var "e2")))
                                                                                                                                         (var "church0?")))))))
                                                                                                                       (var "e1")))
                                                                                                                 (var "church0?")))))))))
                                                                                         (var "Y")))
                                                                                   (lam ["church=?", "$k207"]
                                                                                        (app
                                                                                         (lam ["$f222"]
                                                                                              (app
                                                                                               (lam ["$f230"]
                                                                                                    (app
                                                                                                     (lam ["$e231"]
                                                                                                          (app2
                                                                                                           (var "$f230")
                                                                                                           (var "$e231")
                                                                                                           (lam ["$f224"]
                                                                                                                (app
                                                                                                                 (lam ["$f228"]
                                                                                                                      (app
                                                                                                                       (lam ["$e229"]
                                                                                                                            (app2
                                                                                                                             (var "$f228")
                                                                                                                             (var "$e229")
                                                                                                                             (lam ["$f226"]
                                                                                                                                  (app
                                                                                                                                   (lam ["$e227"]
                                                                                                                                        (app2
                                                                                                                                         (var "$f226")
                                                                                                                                         (var "$e227")
                                                                                                                                         (lam ["$e225"]
                                                                                                                                              (app2
                                                                                                                                               (var "$f224")
                                                                                                                                               (var "$e225")
                                                                                                                                               (lam ["$e223"]
                                                                                                                                                    (app2
                                                                                                                                                     (var "$f222")
                                                                                                                                                     (var "$e223")
                                                                                                                                                     (lam ["$f208"]
                                                                                                                                                          (app
                                                                                                                                                           (lam ["$f216"]
                                                                                                                                                                (app
                                                                                                                                                                 (lam ["$f220"]
                                                                                                                                                                      (app
                                                                                                                                                                       (lam ["$e221"]
                                                                                                                                                                            (app2
                                                                                                                                                                             (var "$f220")
                                                                                                                                                                             (var "$e221")
                                                                                                                                                                             (lam ["$f218"]
                                                                                                                                                                                  (app
                                                                                                                                                                                   (lam ["$e219"]
                                                                                                                                                                                        (app2
                                                                                                                                                                                         (var "$f218")
                                                                                                                                                                                         (var "$e219")
                                                                                                                                                                                         (lam ["$e217"]
                                                                                                                                                                                              (app2
                                                                                                                                                                                               (var "$f216")
                                                                                                                                                                                               (var "$e217")
                                                                                                                                                                                               (lam ["$f210"]
                                                                                                                                                                                                    (app
                                                                                                                                                                                                     (lam ["$f214"]
                                                                                                                                                                                                          (app
                                                                                                                                                                                                           (lam ["$e215"]
                                                                                                                                                                                                                (app2
                                                                                                                                                                                                                 (var "$f214")
                                                                                                                                                                                                                 (var "$e215")
                                                                                                                                                                                                                 (lam ["$f212"]
                                                                                                                                                                                                                      (app
                                                                                                                                                                                                                       (lam ["$e213"]
                                                                                                                                                                                                                            (app2
                                                                                                                                                                                                                             (var "$f212")
                                                                                                                                                                                                                             (var "$e213")
                                                                                                                                                                                                                             (lam ["$e211"]
                                                                                                                                                                                                                                  (app2
                                                                                                                                                                                                                                   (var "$f210")
                                                                                                                                                                                                                                   (var "$e211")
                                                                                                                                                                                                                                   (lam ["$e209"] (app2 (var "$f208") (var "$e209") (var "$k207")))))))
                                                                                                                                                                                                                       (var "church3")))))
                                                                                                                                                                                                           (var "church2")))
                                                                                                                                                                                                     (var "mult")))))))
                                                                                                                                                                                   (var "church1")))))
                                                                                                                                                                       (var "church2")))
                                                                                                                                                                 (var "mult")))
                                                                                                                                                           (var "plus")))))))))
                                                                                                                                   (var "church3")))))
                                                                                                                       (var "church1")))
                                                                                                                 (var "plus")))))
                                                                                                     (var "church2")))
                                                                                               (var "mult")))
                                                                                         (var "church=?")))))))))))))))))))))))))))))

  end (* Examples *)
