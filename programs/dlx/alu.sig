(* alu.sig
 *
 * This defines the exported datatype and function provided by the
 * ALU.  The datatype ALUOp provides a means to specify which
 * operation is to be performed by the ALU, and PerformAL performs
 * one of the operations on two thirty-two bit words, returning the
 * result as a thirty-two bit word.
 *)

signature ALU
  = sig

      datatype ALUOp = SLL | SRL | SRA |
                       ADD | ADDU |
                       SUB | SUBU |
                       AND | OR | XOR |
                       SEQ | SNE |
                       SLT | SGT |
                       SLE | SGE;

      val PerformAL : (ALUOp * Word32.word * Word32.word) -> Word32.word;

    end
