(*
 * From David McClain's language study.
 * http://www.azstarnet.com/~dmcclain/LanguageStudy.html
 *
 * Stephen Weeks replaced Unsafe.Real64Array with Real64Array.
 *)

structure Main : BMARK =
  struct

    val name = "zern"

    val results = []

    fun testit () = (MSpeed.ztest 1; print "OK\n")
        handle _ => print "FAIL\n"

    fun doit () = (
          MSpeed.ztest 6000;
          print ("Ok - result close to 0.219...\n"))

  end
