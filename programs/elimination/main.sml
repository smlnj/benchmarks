(*#line 0.0 "data/preamble.sml"*)
(* ========================================================================= *)
(* SETTING UP THE ENVIRONMENT IN WHICH WE CAN EXECUTE THE METIS PROVER       *)
(* Created by Joe Hurd, September 2001                                       *)
(* ========================================================================= *)

(* Loading the modules we use *)

structure Main : BMARK =
  struct

    val quotation = ref false;
    val installPP = fn (_ : PP.ppstream -> 'a -> unit) => ();

    fun main _ = let
        (* Pretty printers *)
        val () = installPP Term1.pp_term;
        val () = installPP Term1.pp_formula;
        val () = installPP Subst1.pp_subst;
        val () = installPP Thm1.pp_thm;
        (* Parsing quotations *)
        val () = quotation := true;
        (* Creating nice output *)
        local
          fun dup _ 0 l = l | dup x n l = dup x (n - 1) (x :: l);
          fun chs x n = implode (dup x n []);
        in
          fun advertize s = Log.print ("==" ^ s ^ chs #"=" (77 - size s) ^ "\n\n");
          fun separator () = Log.print (chs #"-" 79 ^ "\n\n");
        end;
        fun cutoff max =
          let
            fun cut feas sofa l =
              let val poss = sofa ^ " ... " ^ Useful.int_to_string (length l) ^ " more"
              in cut' (if size poss < max then poss else feas) sofa l
              end
            and cut' _ sofa [] = sofa
              | cut' feas sofa (h :: t) =
              let val sofa' = if sofa = "" then h else sofa ^ " " ^ h
              in if size sofa' < max then cut feas sofa' t else feas
              end
          in
            cut "" ""
          end;
        local
          fun b2s true = "on" | b2s false = "off";
          val i2s = Useful.int_to_string;
          val l2s = Meter1.limit_to_string;
        in
          fun show (settings : Metis1.parameters) =
            let
              val {meson = Mactive, delta = Dactive, resolution = Ractive,
                   meson_parm = Mparm, resolution_parm = Rparm} = settings
            in
              "resolution             = " ^ b2s Ractive ^ "\n" ^
              "meson                  = " ^ b2s Mactive ^ "\n" ^
              "delta                  = " ^ b2s Dactive ^ "\n" ^
              "\n" ^
              "resolution_parm:\n" ^
              "  subsumption_checking = " ^ i2s (#subsumption_checking Rparm) ^ "\n" ^
              "  positive_refinement  = " ^ b2s (#positive_refinement Rparm) ^ "\n" ^
              "  theap_parm:\n" ^
              "    fifo_skew          = " ^ i2s (#fifo_skew (#theap_parm Rparm)) ^ "\n" ^
              "    theap_cleaning     = " ^ i2s (#cleaning_freq (#theap_parm Rparm)) ^"\n"^
              "\n" ^
              "meson_parm:\n" ^
              "  ancestor_pruning     = " ^ b2s (#ancestor_pruning Mparm) ^ "\n" ^
              "  ancestor_cutting     = " ^ b2s (#ancestor_cutting Mparm) ^ "\n" ^
              "  state_simplify       = " ^ b2s (#state_simplify Mparm) ^ "\n" ^
              "  cache_cutting        = " ^ b2s (#cache_cutting Mparm) ^ "\n" ^
              "  divide_conquer       = " ^ b2s (#divide_conquer Mparm) ^ "\n" ^
              "  unit_lemmaizing      = " ^ b2s (#unit_lemmaizing Mparm) ^ "\n" ^
              "\n" ^
              "limit                  = " ^ l2s (!Metis1.limit) ^ "\n\n"
            end;
        end;

        (* The core proving function *)

        val cnf_normalization = ref false;

        fun with_cnf b = Useful.with_flag (cnf_normalization, Useful.K b);

        fun core_prove fm =
          let
            val prover = if !cnf_normalization then Metis1.prove else Metis1.raw_prove
          in
            case Useful.try prover fm of SOME _
              => Log.print "METIS: SUCCESSFULLY PROVED\nMETIS: "
            | NONE => Log.print "METIS: FAILED TO PROVE\nMETIS: "
          end;

        fun process name goal =
          (Log.print ("METIS: Problem " ^ name ^ "\n");
           core_prove goal;
           Log.print "\n");

        fun process_set (n, s) =
          let
            val () = advertize n
            fun f {name, goal} = process name (Term1.parse_formula goal)
          in
            case s of [] => ()
            | p :: ps => (f p; app (fn x => (separator (); f x)) ps)
          end;

        (* Get options from the command line *)

        local
          open Useful Metis1;

          fun tlimit "-" = NONE | tlimit s = SOME (Real.fromInt (string_to_int s));

          fun opts [] = 0
            | opts (x :: xs) =
            if x = "-t" orelse x = "--time" then
              case xs of [] => raise Fail "options: last argument -t / --time"
              | y :: ys => (limit := {time = tlimit y, infs = NONE}; opts ys)
            else if x = "-m" orelse x = "--meson" then
              (settings := update_parm_meson not (!settings); opts xs)
            else if x = "-r" orelse x = "--resolution" then
              (settings := update_parm_resolution not (!settings); opts xs)
            else if x = "-d" orelse x = "--delta" then
              (settings := update_parm_delta not (!settings); opts xs)
            else if x = "--" then length xs
            else if hd (explode x) = #"-" then raise Fail ("unknown parameter: " ^ x)
            else 1 + length xs;
        in
          fun options () =
            let
              val () = settings := update_parm_resolution (K false) (!settings);
              val () = settings := update_parm_meson      (K false) (!settings);
              val () = settings := update_parm_delta      (K false) (!settings);

              val l = (**CommandLine.arguments ()**) []
              val n = opts l
            in
              split l (length l - n)
            end;
        end;

        val (opts, work) = options ()
        (*#line 0.0 "data/benchmark.sml"*)
        val pure = null ((**CommandLine.arguments ()**) []);

        local
          open Useful Metis1;
        in
          val () =
            if pure then settings:= update_parm_meson (K true) (!settings) else ();
        end;

        local
          open Useful Problem1;
          fun extract p n =
             (Option.valOf o List.find (fn {name, goal = _} => name = n)) p;

          val meson_prune =
            if pure then ["P29", "LDA007-3", "GRP010-4", "GEO002-4"] else ["GEO002-4"];

          val prune =
            let
              val {meson, resolution, ...} = !Metis1.settings
            in
              (fn f => List.filter (not o f))
              (case (meson, resolution) of (false, false) => K true
               | (false, true) => C mem ["COL060-3"]
               | (true, false) => C mem meson_prune
               | (true, true) => K false)
            end;

          val src0 = ["P26", "P29", "P46", "GILMORE_1", "LOS", "STEAM_ROLLER"];

          val src1 = ["P48", "P49", "AGATHA"];

          val src2 =
            ["LCL009-1", "COL060-3", "COL058-2", "LCL107-1", "LDA007-3",
             "GRP010-4", "BOO021-1", "GEO002-4", "GRP128-4.003"];
        in
          val set0 = map (extract nonequality) (prune src0);
          val set1 = map (extract equality)    (prune src1);
          val set2 = map (extract tptp)        (prune src2);
        end;

        val program = "benchmark" ^ (if pure then "*" else "");

        val () = advertize (program ^ "==starting");

        val () = advertize "settings";

        val () = Log.print (show (!Metis1.settings));

        val () = with_cnf true process_set ("nonequality", set0);

        val () = with_cnf true process_set ("equality", set1);

        val () = with_cnf false process_set ("tptp", set2);

        val () = advertize (program ^ "==finishing");

        in
          ()
        end; (* main *)

    val name = "elimination"

    val results = []

    fun testit () = main()

    fun doit () = main()

  end (* Main *)
