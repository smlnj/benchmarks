(* timeit.sml
 *
 * COPYRIGHT (c) 2021 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Timing : sig

    (* measure compile time by timing the "use" function on a file *)
    val timeUse : TextIO.outstream * string -> unit

    (* measure compile time by timing the "CM.make" function on a file *)
    val timeMake : TextIO.outstream * string -> unit

    (* time the running of a function *)
    val timeIt : TextIO.outstream * (unit -> 'a) -> unit

    (* report the GC statistics for running a function *)
    val gcStats : TextIO.outstream * (unit -> 'a) -> unit

    (* run a function multiple times, reporting the result as a labeled
     * JSON array of timing records.
     *)
    val time : int * TextIO.outstream * (unit -> 'a) -> unit

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

    fun timeUse (outstrm, file) = let
	  val t0 = start()
	  in
	    use file;
	    output (outstrm, stop t0);
	    TextIO.output1 (outstrm, #"\n")
	  end

    fun timeMake (outstrm, file) = let
	  val t0 = start()
	  in
	    CM.make file;
	    output (outstrm, stop t0);
	    TextIO.output1 (outstrm, #"\n")
	  end

    fun timeIt (outstrm, doit) = let
	  val t0 = start()
	  in
	    doit();
	    TextIO.output1 (outstrm, #"\t");
	    output (outstrm, stop t0);
	    TextIO.flushOut outstrm
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
    fun gcStats (outstrm, doit) = let
          val () = reset true
          val _ = doit()
          val {nbAlloc, nbAlloc1, nbPromote, nGCs} = read()
          in
            TextIO.output (outstrm, String.concat[
                 "{ \"nursery-alloc\" : ", LargeInt.toString nbAlloc,
                 ", \"gen1-alloc\" : ", LargeInt.toString nbAlloc1,
                 ", \"gen1-promote\" : ", LargeInt.toString nbPromote,
                 ", \"num-gcs\" : [", String.concatWithMap "," Int.toString nGCs, "] }"
               ])
          end
    end (* local *)

    fun time (n, outstrm, doit) = let
	  fun loop 0 = ()
	    | loop 1 = timeIt(outstrm, doit)
	    | loop i = (
		timeIt(outstrm, doit);
		TextIO.output(outstrm, ",\n");
		loop(i-1))
	  in
	    TextIO.output (outstrm, "    \"runs\" : [\n");
	    loop n;
	    TextIO.output (outstrm, "      ]")
	  end

  end
