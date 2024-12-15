(* sim-stuff.sml
 *)

structure SimStuff =
struct

  val sub = Array.sub
  infix 9 sub


fun read file =
    let val if1 = (TextIO.openIn "simprelude.s")
        val if2 = (TextIO.openIn file)
        val if3 = (TextIO.openIn "simpostlude.s")
        val prelude = ReadAbs.read if1
        val prog = ReadAbs.read if2
        val postlude = ReadAbs.read if3
    in
        TextIO.closeIn if1;
        TextIO.closeIn if2;
        TextIO.closeIn if3;
        prelude @ prog @ postlude
    end

fun init file = SetEnv.init (read file)

val runcount = ref 0

fun run ()=
    let open AbsMach
        val foo = runcount := 0
        fun updc NOP = runcount := !runcount + 1
          | updc _ = ()
        open SetEnv
        fun f () = (step(); (updc o hd o pc)(); f())
    in
        f()
    end

fun srun () = let open SetEnv in d_pc(); step(); srun() end;

fun memsave () = !SetEnv.Memory


fun memcmp(a:AbsMach.values array, b:AbsMach.values array) =
    let open AbsMach
        fun cmp (INT a, INT b) = a = b
          | cmp (REAL a, REAL b) = Real.==(a, b)
          | cmp (LABVAL _, LABVAL _) = true
          | cmp _ = false
        fun f 0 = ~1
          | f n = if cmp((a sub n), (b sub n)) then f (n - 1) else n
        val al = Array.length a
        val bl = Array.length b
    in
        if al = bl then f (al - 1) else (print "size\n"; 0)
    end


fun copyarray a = Array.tabulate (Array.length a, fn i => a sub i)

exception PROG_NO_END

local open AbsMach
in
    fun vstring (INT i) = "INT " ^ Int.toString i
      | vstring (REAL i) = "REAL " ^ Real.toString i
      | vstring (LABVAL(i, j)) =
        "LABVAL(" ^ Int.toString i ^ ", " ^ Int.toString j ^ ")"
end

fun runf f =
    ((init f;
      run ();
      raise PROG_NO_END))
    handle End_of_Program => (print "eop\n";
                              SetEnv.regc 4)


fun cmprog(f1, f2) =
    let open AbsMach
        fun intof (INT i) = i
        fun ptsat p = SetEnv.mcell (intof p)
        val p1 = runf f1
        (* val foo = print ("cmprog1:" ^ vstring p1 ^ "\n") *)
        val v1 = ptsat p1
        val r1 = !runcount
        val p2 = runf f2
        (* val foo = print ("cmprog2:" ^ vstring p2 ^ "\n") *)
        val v2 = ptsat p2
        val r2 = !runcount

    in
        (f1 ^ " ct " ^ Int.toString r1 ^ " ptr " ^ vstring p1 ^
          " val " ^ vstring v1 ^
         f2 ^ " ct " ^ Int.toString r2 ^ " ptr " ^ vstring p2 ^
         " val " ^ vstring v2 ^  "\n")
    end

end
