(* alu.sml
 *
 * This defines the ALU structure, which provides the functionality of
 * an Arithmetic/Logic Unit.  The datatype ALUOp provides a means to
 * specify which operation is to be performed by the ALU, and
 * PerformAL performs one of the operations on two thirty-two bit
 * words, returning the result as a thirty-two bit word.
 *
 * A note about SML'97 Basis Library implementation of thirty-two bit
 * numbers: the Word32.word is an unsigned thirty-two bit integer,
 * while Int.int (equivalent to Int.int) is a signed thirty-two
 * bit integer.  In order to perform the signed operations, it is
 * necessary to convert the words to signed form, using the
 * Word32.toIntX function, which performs sign extension,
 * and to convert the result back into unsigned form using the
 * Word32.fromInt function.  In addition, to perform a shift,
 * the second Word32.word needs to be "downsized" to a normal
 * Word.word using the Word.fromWord function.
 *)

structure ALU : ALU
  = struct

      datatype ALUOp = SLL | SRL | SRA |
                       ADD | ADDU |
                       SUB | SUBU |
                       AND | OR | XOR |
                       SEQ | SNE |
                       SLT | SGT |
                       SLE | SGE;

      fun PerformAL (opcode, s1, s2) =
        (case opcode
           of SLL =>
                Word32.<< (s1, Word.fromLarge (Word32.toLarge s2))
            | SRL =>
                Word32.>> (s1, Word.fromLarge (Word32.toLarge s2))
            | SRA =>
                Word32.~>> (s1, Word.fromLarge (Word32.toLarge s2))
            | ADD =>
                Word32.fromInt (Int.+ (Word32.toIntX s1,
                                                 Word32.toIntX s2))
            | ADDU =>
                Word32.+ (s1, s2)
            | SUB =>
                Word32.fromInt (Int.- (Word32.toIntX s1,
                                                 Word32.toIntX s2))
            | SUBU =>
                Word32.- (s1, s2)
            | AND =>
                Word32.andb (s1, s2)
            | OR =>
                Word32.orb (s1, s2)
            | XOR =>
                Word32.xorb (s1, s2)
            | SEQ =>
                if (s1 = s2)
                  then 0wx00000001 : Word32.word
                  else 0wx00000000 : Word32.word
            | SNE =>
                if not (s1 = s2)
                  then 0wx00000001 : Word32.word
                  else 0wx00000000 : Word32.word
            | SLT =>
                if Int.< (Word32.toIntX s1, Word32.toIntX s2)
                  then 0wx00000001 : Word32.word
                  else 0wx00000000 : Word32.word
            | SGT =>
                if Int.> (Word32.toIntX s1, Word32.toIntX s2)
                  then 0wx00000001 : Word32.word
                  else 0wx00000000 : Word32.word
            | SLE =>
                if Int.<= (Word32.toIntX s1, Word32.toIntX s2)
                  then 0wx00000001 : Word32.word
                  else 0wx00000000 : Word32.word
            | SGE =>
                if Int.>= (Word32.toIntX s1, Word32.toIntX s2)
                  then 0wx00000001 : Word32.word
                  else 0wx00000000 : Word32.word)
           (*
            * This handle will handle all ALU errors, most
            * notably overflow and division by zero, and will
            * print an error message and return 0.
            *)
           handle _ =>
             (print "Error : ALU returning 0\n";
              0wx00000000 : Word32.word);

    end
