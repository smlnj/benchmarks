(* timeit.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Timing : sig

    (* measure compile time by timing the "use" function on a file *)
    val timeUse : string -> TextIO.outstream -> unit

    (* measure compile time by timing the "CM.make" function on a file *)
    val timeMake : string -> TextIO.outstream -> unit

    (* time the running of a function *)
    val timeIt : (unit -> 'a) -> TextIO.outstream -> unit

    (* report the GC statistics for running a function; note that this measurement
     * requires version 110.99.7+ or 2024.3+
     *)
    val gcStats : (unit -> 'a) -> TextIO.outstream -> unit

    val runOnce : string * (TextIO.outstream -> unit) -> unit

    (* `run (name, label, n, file, f)` opens `file` for appending and then calls
     * the function `f` on it `n` times.
     *)
    val run : string * string * int * string * (TextIO.outstream -> unit) -> unit

  end = struct

    structure TR = Timer
    structure T = Time

    type timing = {usr:T.time, gc:T.time, sys:T.time, real:T.time}

    fun pad (s, n) = StringCvt.padLeft #" " n s

    fun start () = (
	  SMLofNJ.Internals.GC.doGC 1000;
         {realt = TR.startRealTimer(), timer = TR.startCPUTimer()})

    fun stop {realt, timer} = let
          val rt = TR.checkRealTimer realt
          val {usr, sys} = TR.checkCPUTimer timer
	  val gc = TR.checkGCTime timer
	  in
	    {usr=usr, gc=gc, sys=sys, real=rt}
	  end

    (* convert a time value to a string, padded on the left to 8 characters *)
    fun timeToStr time = pad (Time.toString time, 6)

    (* output timing statistics as a JSON object *)
    fun output (strm, {usr, gc, sys, real} : timing) =
	  TextIO.output (strm, String.concat[
               "{\"usr\" : ", timeToStr usr,
               ", \"sys\" : ", timeToStr sys,
               ", \"gc\" : ", timeToStr gc,
               ", \"real\" : ", timeToStr real, "}"
             ])

    fun timeUse file outS = let
	  val t0 = start()
	  in
	    use file;
	    output (outS, stop t0);
	    TextIO.flushOut outS
	  end

    fun timeMake file outS = let
	  val t0 = start()
	  in
	    CM.make file;
	    output (outS, stop t0);
	    TextIO.flushOut outS
	  end

    fun timeIt doit outS = let
	  val t0 = start()
	  in
	    doit();
	    TextIO.output1 (outS, #"\t");
	    output (outS, stop t0);
	    TextIO.flushOut outS
	  end

    local
(* NOTE: these functions will eventually be exposed via the SMLofNJ.Internal.GC
 * structure.
 *)
      structure CI = Unsafe.CInterface
      val read' : unit -> word * word * word * word * word list =
            (CI.c_function "SMLNJ-RunT" "gcCounterRead")
              handle CI.CFunNotFound _ => (
                TextIO.output(TextIO.stdErr, "Warning: gcCounterRead not found\n");
                fn _ => raise Fail "gcCounterRead not available")
      val reset : bool -> unit =
            (CI.c_function "SMLNJ-RunT" "gcCounterReset")
              handle CI.CFunNotFound _ => (
                TextIO.output(TextIO.stdErr, "Warning: gcCounterReset not found\n");
                fn _ => raise Fail "gcCounterReset not available")
      fun read () = let
            (* results are:
             *   s     -- scaling factor for allocation counts
             *   a     -- scaled nursery allocation count
             *   a1    -- scaled first-generation allocation count
             *   p     -- scaled count of promotions to first generation
             *   ngcs  -- number of collections by generation
             *)
            val (s, a, a1, p, ngcs) = read'()
            val scale = Word.toLargeInt s
            in {
              nbAlloc = scale * Word.toLargeInt a,
              nbAlloc1 = scale * Word.toLargeInt a1,
              nbPromote = scale * Word.toLargeInt p,
              nGCs = List.map Word.toIntX ngcs
            } end
    in
    fun gcStats doit outS = let
          val () = reset true
          val _ = doit()
          val {nbAlloc, nbAlloc1, nbPromote, nGCs} = read()
          in
            TextIO.output (outS, String.concat[
                 "{ \"nursery-alloc\" : ", LargeInt.toString nbAlloc,
                 ", \"gen1-alloc\" : ", LargeInt.toString nbAlloc1,
                 ", \"gen1-promote\" : ", LargeInt.toString nbPromote,
                 ", \"num-gcs\" : [", String.concatWithMap "," Int.toString nGCs, "] }"
               ])
          end
    end (* local *)

    fun runOnce (outfile, doit) = let
          val outS = TextIO.openAppend outfile
          in
            doit outS;
            TextIO.closeOut outS
          end

    fun run (name, label, nruns, outfile, doit) = let
          val outS = TextIO.openAppend outfile
          fun pr s = TextIO.output(outS, s)
          fun loop 0 = ()
	    | loop 1 = (
                doit outS;
                pr "\n")
	    | loop i = (
                pr "    ";
		doit outS;
		pr ",\n";
		loop(i-1))
          in
            pr(concat["{ \"program\" : \"", name, "\","]);
            if (nruns = 1)
              then (
                pr(concat["  \"", label, "\" : "]);
                doit outS; pr "\n")
              else (
                pr(concat["  \"", label, "\" : [\n"]);
                loop nruns;
                pr "  ]\n}\n");
            TextIO.closeOut outS
          end

  end
