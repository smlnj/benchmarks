(* main.sml
 *)

structure Main : BMARK =
  struct

    val name = "dlx"


    fun run optOut (instructions, inputs) = let
          fun inputFn {state = (inputs, outputs)} = (case inputs
                 of [] => {input = 0, state = ([], outputs)}
                  | input::inputs => {input = input, state = (inputs, outputs)}
                (* end case *))
          fun outputFn {output, state = (inputs, outputs)} =
                {state = (inputs, output::outputs)}
          val state = (inputs, [])
          val trap = {inputFn = inputFn, outputFn = outputFn, state = state}
          val {state = (_, outputs), statistics} =
                DLXSimulatorC1.run_prog {instructions = instructions, trap = trap}
          in
            case optOut
             of SOME outS => let
                  fun pr s = TextIO.output(outS, s)
                  in
                    List.app
                      (fn output => pr (concat["Output: ", Int.toString output, "\n"]))
                        outputs;
                    pr (statistics ());
                    pr "\n"
                  end
              | NONE => ()
            (* end case *)
          end

    fun runAll optOut = List.app (run optOut) [
            (Examples.Simple, []),
            (Examples.Twos, [10]),
            (Examples.Abs, [~10]),
            (Examples.Fact, [12]),
            (Examples.GCD, [123456789,98765])
          ]

    fun testit outS = runAll (SOME outS)

    fun doit () = let
          fun loop n = if n = 0
                then ()
                else (runAll NONE; loop (n-1))
          in
            loop 5
          end

  end
