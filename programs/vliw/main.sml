(* main.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure Main : BMARK =
  struct

    val name = "vliw"

    val _ = (
          Node.move_test_debug := false;
          Node.move_op_debug := false;
          Node.rw_debug := false;
          Node.delete_debug := false;
          Node.ntn_debug := true;
          Node.prog_node_debug := false;
          Node.prog_node_debug_verbose := false;
          Node.closure_progs_debug := false;
          Node.cpsiCheck := false;
          Compress.compress_debug := false;
          ReadI.read_debug := false;
          ReadI.write_debug := false;
          ReadI.live_debug := false)

    fun writeprog(file, j, p) = let
          val ot = (TextIO.openOut file)
          val prog = ReadI.writeI(j, p)
          val filp = (Delay.rm_bogus o OutFilter.remnops) prog
          val _ = PrintAbs.show ot filp
          in
            TextIO.closeOut ot
          end;

    fun wp(file, prog) = let
          val ot = (TextIO.openOut file)
          val filp = Delay.rm_bogus prog
          val xxx = PrintAbs.show ot filp
          in
            TextIO.closeOut ot
          end;

    fun dodelay i = (Delay.init i; Delay.add_delay i);

    val ifile = "DATA/ndotprod.s" (* input *)
    val ofile = "DATA/tmp.s" (* output *)
    val c_ofile = "DATA/cmp.s" (* compressed output *)

    fun run (ifile:string, ofile:string, c_ofile:string, ws:int) = let
          val foo = Ntypes.init_names()
          val i = (dodelay o BreakInst.breaki o ReadAbs.read o TextIO.openIn) ifile
          (* building nodes *)
          val (j, p) = ReadI.readI i
          (* writing unopt *)
          val () = writeprog(ofile, j, p)
          fun cwin p = Compress.compress(ws, p)
          (* compressing program *)
          val cp = map cwin p
          (* writing opt program *)
          val () = writeprog(c_ofile, j, cp)
          in
            Ntypes.new_name "0"
          end

    fun testit outS = let
          val idemp = 0
          val ws = 9
          val () = (
                Delay.idempotency := idemp;
                print (concat[
                    "compressing ", ifile, " into (uncompressed) ", ofile,
                    " and (compressed) ", c_ofile,
                    " with idempotency ", Int.toString idemp,
                    " and window size ", Int.toString ws, "\n"
                  ]))
          val code_motions = run (ifile, ofile, c_ofile, ws)
(* the following produces a Subscript exception in `set-env.sml` (line 370)
          val answer = SimStuff.cmprog(ofile, c_ofile)
*)
          val answer = ""
          in
            print (answer ^ "code_motions " ^ code_motions ^ " \n")
          end

    fun runOne () = (
          Delay.idempotency := 0;
          ignore (run (ifile, ofile, c_ofile, 9)))

    fun loop 0 = ()
      | loop n = (runOne(); loop (n-1))

    fun doit () = loop 250

  end
