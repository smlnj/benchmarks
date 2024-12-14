(* dlx.fun
 *
 * This defines the DLXSimulatorFun functor, which takes three
 * structures, corresponding to the register file, the ALU, and memory,
 * and provides the functionality of a DLX processor, able to execute
 * DLX programs.  The function run_file takes a string corresponding to the
 * name of the file to be executed, and executes it.  The function
 * run_prog takes a list of instructions and executes them.
 *)

functor DLXSimulatorFun (structure RF : REGISTERFILE;
                         structure ALU : ALU;
                         structure MEM : MEMORY; ) : DLXSIMULATOR
  = struct

      (*
       * The datatype Opcode provides a means of differentiating *
       * among the main opcodes.
       *)
      datatype Opcode =
        (* for R-type opcodes *)
        SPECIAL |
        (* I-type opcodes *)
        BEQZ | BNEZ |
        ADDI | ADDUI | SUBI | SUBUI |
        ANDI | ORI | XORI |
        LHI |
        SLLI | SRLI | SRAI |
        SEQI | SNEI | SLTI | SGTI | SLEI | SGEI |
        LB | LBU | SB |
        LH | LHU | SH |
        LW | SW |
        (* J-type opcodes *)
        J | JAL | TRAP | JR | JALR |
        (* Unrecognized opcode *)
        NON_OP;

      (*
       * The datatype RRFuncCode provides a means of
       * differentiating among
       * the register-register function codes.
       *)
      datatype RRFunctCode = NOP | SLL | SRL | SRA |
                             ADD | ADDU | SUB | SUBU |
                             AND | OR | XOR |
                             SEQ | SNE | SLT | SGT | SLE | SGE |
                             NON_FUNCT;

      (*
       * The datatype Instruction provides a means of
       * differentiating among the three different types of
       * instructions, I-type, R-type, and J-type.
       * An I-type is interpreted as (opcode, rs1, rd, immediate).
       * An R-type is interpreted as (opcode, rs1, rs2, rd, shamt, funct).
       * An J-type is interpreted as (opcode, offset).
       * An ILLEGAL causes the simulator to end.
       *)
      datatype Instruction
        = ITYPE of Opcode * int * int * Word32.word
        | RTYPE of Opcode * int * int * int * int * RRFunctCode
        | JTYPE of Opcode * Word32.word
        | ILLEGAL;

      (*
       * The value HALT is set to the DLX instruction TRAP #0,
       * and is used to check for the halt of the program.
       *)
      val HALT = JTYPE (TRAP, 0wx00000000);

      (*
       * The function DecodeIType decodes a Word32.word into an
       * I-type instruction.
       *)
      fun DecodeIType instr
          = let
              val opc = Word32.andb (Word32.>> (instr,
                                                0wx001A),
                                     0wx0000003F : Word32.word);

              val opcode = case opc
                             of 0wx00000004 : Word32.word => BEQZ
                              | 0wx00000005 : Word32.word => BNEZ
                              | 0wx00000008 : Word32.word => ADDI
                              | 0wx00000009 : Word32.word => ADDUI
                              | 0wx0000000A : Word32.word => SUBI
                              | 0wx0000000B : Word32.word => SUBUI
                              | 0wx0000000C : Word32.word => ANDI
                              | 0wx0000000D : Word32.word => ORI
                              | 0wx0000000E : Word32.word => XORI
                              | 0wx0000000F : Word32.word => LHI
                              | 0wx00000014 : Word32.word => SLLI
                              | 0wx00000016 : Word32.word => SRLI
                              | 0wx00000017 : Word32.word => SRAI
                              | 0wx00000018 : Word32.word => SEQI
                              | 0wx00000019 : Word32.word => SNEI
                              | 0wx0000001A : Word32.word => SLTI
                              | 0wx0000001B : Word32.word => SGTI
                              | 0wx0000001C : Word32.word => SLEI
                              | 0wx0000001D : Word32.word => SGEI
                              | 0wx00000020 : Word32.word => LB
                              | 0wx00000024 : Word32.word => LBU
                              | 0wx00000028 : Word32.word => SB
                              | 0wx00000021 : Word32.word => LH
                              | 0wx00000025 : Word32.word => LHU
                              | 0wx00000029 : Word32.word => SH
                              | 0wx00000023 : Word32.word => LW
                              | 0wx0000002B : Word32.word => SW
                              | _ => (print "Error : Non I-Type opcode\n";
                                      NON_OP);

              val rs1 = Word32.toInt(Word32.andb (Word32.>> (instr, 0wx0015),
                                                  0wx0000001F : Word32.word));

              val rd = Word32.toInt(Word32.andb (Word32.>> (instr, 0wx0010),
                                                 0wx0000001F : Word32.word));

              val immediate = Word32.~>> (Word32.<< (instr, 0wx0010),
                                          0wx0010);

            in
              if opcode = NON_OP
                then ILLEGAL
                else ITYPE (opcode, rs1, rd, immediate)
            end;

      (*
       * The function DecodeRType decodes a Word32.word into an
       * R-type instruction.
       *)
      fun DecodeRType instr
          = let

              val rs1 = Word32.toInt (Word32.andb (Word32.>> (instr, 0wx0015),
                                                   0wx0000001F : Word32.word));

              val rs2 = Word32.toInt (Word32.andb (Word32.>> (instr, 0wx0010),
                                                   0wx0000001F : Word32.word));

              val rd = Word32.toInt (Word32.andb (Word32.>> (instr, 0wx000B),
                                                  0wx0000001F : Word32.word));

              val shamt
                  = Word32.toInt (Word32.andb (Word32.>> (instr, 0wx0006),
                                               0wx0000001F : Word32.word));

              val funct = Word32.andb (instr, 0wx0000003F : Word32.word);

              val functcode = case funct
                                of 0wx00000000 : Word32.word => NOP
                                 | 0wx00000004 : Word32.word => SLL
                                 | 0wx00000006 : Word32.word => SRL
                                 | 0wx00000007 : Word32.word => SRA
                                 | 0wx00000020 : Word32.word => ADD
                                 | 0wx00000021 : Word32.word => ADDU
                                 | 0wx00000022 : Word32.word => SUB
                                 | 0wx00000023 : Word32.word => SUBU
                                 | 0wx00000024 : Word32.word => AND
                                 | 0wx00000025 : Word32.word => OR
                                 | 0wx00000026 : Word32.word => XOR
                                 | 0wx00000028 : Word32.word => SEQ
                                 | 0wx00000029 : Word32.word => SNE
                                 | 0wx0000002A : Word32.word => SLT
                                 | 0wx0000002B : Word32.word => SGT
                                 | 0wx0000002C : Word32.word => SLE
                                 | 0wx0000002D : Word32.word => SGE
                                 | _ => (print "Error : Non R-type funct\n";
                                         NON_FUNCT);

            in
              if functcode = NON_FUNCT
                then ILLEGAL
                else RTYPE (SPECIAL, rs1, rs2, rd, shamt, functcode)
            end;

      (*
       * The function DecodeJType decodes a Word32.word into an
       * J-type instruction.
       *)
      fun DecodeJType instr
          = let

              val opc = Word32.andb (Word32.>> (instr, 0wx1A),
                                     0wx0000003F : Word32.word);

              val opcode = case opc
                             of 0wx00000002 : Word32.word => J
                              | 0wx00000003 : Word32.word => JAL
                              | 0wx00000011 : Word32.word => TRAP
                              | 0wx00000012 : Word32.word => JR
                              | 0wx00000013 : Word32.word => JALR
                              | _ => (print "Error : Non J-type opcode\n";
                                      NON_OP);

              val offset = Word32.~>> (Word32.<< (instr, 0wx0006),
                                       0wx0006);

            in
                if opcode = NON_OP
                    then ILLEGAL
                    else JTYPE (opcode, offset)
            end;

      (*
       * The function DecodeInstr decodes a Word32.word into an
       * instruction.  It first checks the opcode, and then calls
       * one of DecodeIType, DecodeJType, and DecodeRType to
       * complete the decoding process.
       *)
      fun DecodeInstr instr
          = let

              val opcode = Word32.andb (Word32.>> (instr, 0wx1A),
                                        0wx0000003F : Word32.word);

            in
              case opcode
                of 0wx00000000 : Word32.word => DecodeRType instr
                 | 0wx00000002 : Word32.word => DecodeJType instr
                 | 0wx00000003 : Word32.word => DecodeJType instr
                 | 0wx00000004 : Word32.word => DecodeIType instr
                 | 0wx00000005 : Word32.word => DecodeIType instr
                 | 0wx00000008 : Word32.word => DecodeIType instr
                 | 0wx00000009 : Word32.word => DecodeIType instr
                 | 0wx0000000A : Word32.word => DecodeIType instr
                 | 0wx0000000B : Word32.word => DecodeIType instr
                 | 0wx0000000C : Word32.word => DecodeIType instr
                 | 0wx0000000D : Word32.word => DecodeIType instr
                 | 0wx0000000E : Word32.word => DecodeIType instr
                 | 0wx0000000F : Word32.word => DecodeIType instr
                 | 0wx00000011 : Word32.word => DecodeJType instr
                 | 0wx00000012 : Word32.word => DecodeJType instr
                 | 0wx00000013 : Word32.word => DecodeJType instr
                 | 0wx00000016 : Word32.word => DecodeIType instr
                 | 0wx00000017 : Word32.word => DecodeIType instr
                 | 0wx00000018 : Word32.word => DecodeIType instr
                 | 0wx00000019 : Word32.word => DecodeIType instr
                 | 0wx0000001A : Word32.word => DecodeIType instr
                 | 0wx0000001B : Word32.word => DecodeIType instr
                 | 0wx0000001C : Word32.word => DecodeIType instr
                 | 0wx0000001D : Word32.word => DecodeIType instr
                 | 0wx00000020 : Word32.word => DecodeIType instr
                 | 0wx00000024 : Word32.word => DecodeIType instr
                 | 0wx00000028 : Word32.word => DecodeIType instr
                 | 0wx00000021 : Word32.word => DecodeIType instr
                 | 0wx00000025 : Word32.word => DecodeIType instr
                 | 0wx00000029 : Word32.word => DecodeIType instr
                 | 0wx00000023 : Word32.word => DecodeIType instr
                 | 0wx0000002B : Word32.word => DecodeIType instr
                 | _ => (print "Error : Unrecognized opcode\n";
                         ILLEGAL)
            end;


      (*
       * The function PerformIType performs one of the I-Type
       * instructions.  A number of the instructions make use of the
       * ALU, and as such, call ALU.PerformAL.
       *)
      fun PerformIType ((BEQZ, rs1, rd, immediate), (PC, rf, mem, trap))
        = if (RF.LoadRegister(rf, rs1) = (0wx00000000 : Word32.word))
            then (Word32.fromInt (Int.+ (Word32.toIntX PC,
                                                Word32.toIntX
                                                (Word32.<< (immediate,
                                                            0wx0002)))),
                  rf, mem, trap)
            else (PC, rf, mem, trap)

        | PerformIType ((BNEZ, rs1, rd, immediate), (PC, rf, mem, trap))
          = if not (RF.LoadRegister(rf, rs1) = (0wx00000000 : Word32.word))
              then (Word32.fromInt (Int.+ (Word32.toIntX PC,
                                                  Word32.toIntX
                                                  (Word32.<< (immediate,
                                                              0wx0002)))),
                    rf, mem, trap)
              else (PC, rf, mem, trap)

        | PerformIType ((ADDI, rs1, rd, immediate), (PC, rf, mem, trap))
          = (PC,
             RF.StoreRegister(rf, rd,
                              ALU.PerformAL(ALU.ADD,
                                            RF.LoadRegister(rf, rs1),
                                            immediate)),
             mem, trap)

        | PerformIType ((ADDUI, rs1, rd, immediate), (PC, rf, mem, trap))
          = (PC,
             RF.StoreRegister(rf, rd,
                              ALU.PerformAL(ALU.ADDU,
                                            RF.LoadRegister(rf, rs1),
                                            immediate)),
             mem, trap)

        | PerformIType ((SUBI, rs1, rd, immediate), (PC, rf, mem, trap))
          = (PC,
             RF.StoreRegister(rf, rd,
                              ALU.PerformAL(ALU.SUB,
                                            RF.LoadRegister(rf, rs1),
                                            immediate)),
             mem, trap)

        | PerformIType ((SUBUI, rs1, rd, immediate), (PC, rf, mem, trap))
          = (PC,
             RF.StoreRegister(rf, rd,
                              ALU.PerformAL(ALU.SUBU,
                                            RF.LoadRegister(rf, rs1),
                                            immediate)),
             mem, trap)

        | PerformIType ((ANDI, rs1, rd, immediate), (PC, rf, mem, trap))
          = (PC,
             RF.StoreRegister(rf, rd,
                              ALU.PerformAL(ALU.AND,
                                            RF.LoadRegister(rf, rs1),
                                            immediate)),
             mem, trap)

        | PerformIType ((ORI, rs1, rd, immediate), (PC, rf, mem, trap))
          = (PC,
             RF.StoreRegister(rf, rd,
                              ALU.PerformAL(ALU.OR,
                                            RF.LoadRegister(rf, rs1),
                                            immediate)),
             mem, trap)

        | PerformIType ((XORI, rs1, rd, immediate), (PC, rf, mem, trap))
          = (PC,
             RF.StoreRegister(rf, rd,
                              ALU.PerformAL(ALU.XOR,
                                            RF.LoadRegister(rf, rs1),
                                            immediate)),
             mem, trap)

        | PerformIType ((LHI, rs1, rd, immediate), (PC, rf, mem, trap))
          = (PC, RF.StoreRegister(rf, rd, Word32.<< (immediate, 0wx0010)), mem, trap)

        | PerformIType ((SLLI, rs1, rd, immediate), (PC, rf, mem, trap))
          = (PC, RF.StoreRegister(rf, rd,
                                  Word32.<< (RF.LoadRegister(rf, rs1),
                                             Word.fromLarge (Word32.toLarge immediate))),
             mem, trap)

        | PerformIType ((SRLI, rs1, rd, immediate), (PC, rf, mem, trap))
          = (PC, RF.StoreRegister(rf, rd,
                                  Word32.>> (RF.LoadRegister(rf, rs1),
                                             Word.fromLarge (Word32.toLarge immediate))),
             mem, trap)

        | PerformIType ((SRAI, rs1, rd, immediate), (PC, rf, mem, trap))
          = (PC, RF.StoreRegister(rf, rd,
                                  Word32.~>> (RF.LoadRegister(rf, rs1),
                                              Word.fromLarge (Word32.toLarge immediate))),
             mem, trap)

        | PerformIType ((SEQI, rs1, rd, immediate), (PC, rf, mem, trap))
          = (PC,
             RF.StoreRegister(rf, rd,
                              ALU.PerformAL(ALU.SEQ,
                                            RF.LoadRegister(rf, rs1),
                                            immediate)),
             mem, trap)

        | PerformIType ((SNEI, rs1, rd, immediate), (PC, rf, mem, trap))
          = (PC,
             RF.StoreRegister(rf, rd,
                              ALU.PerformAL(ALU.SNE,
                                            RF.LoadRegister(rf, rs1),
                                            immediate)),
             mem, trap)

        | PerformIType ((SLTI, rs1, rd, immediate), (PC, rf, mem, trap))
          = (PC,
             RF.StoreRegister(rf, rd,
                              ALU.PerformAL(ALU.SLT,
                                            RF.LoadRegister(rf, rs1),
                                            immediate)),
             mem, trap)

        | PerformIType ((SGTI, rs1, rd, immediate), (PC, rf, mem, trap))
          = (PC,
             RF.StoreRegister(rf, rd,
                              ALU.PerformAL(ALU.SGT,
                                            RF.LoadRegister(rf, rs1),
                                            immediate)),
             mem, trap)

        | PerformIType ((SLEI, rs1, rd, immediate), (PC, rf, mem, trap))
          = (PC,
             RF.StoreRegister(rf, rd,
                              ALU.PerformAL(ALU.SLE,
                                            RF.LoadRegister(rf, rs1),
                                            immediate)),
             mem, trap)

        | PerformIType ((SGEI, rs1, rd, immediate), (PC, rf, mem, trap))
          = (PC,
             RF.StoreRegister(rf, rd,
                              ALU.PerformAL(ALU.SGE,
                                            RF.LoadRegister(rf, rs1),
                                            immediate)),
             mem, trap)

        | PerformIType ((LB, rs1, rd, immediate), (PC, rf, mem, trap))
          = let
              val (nmem, l_byte)
                  = MEM.LoadByte(mem, Word32.+ (RF.LoadRegister(rf, rs1),
                                                immediate));
            in
              (PC,
               RF.StoreRegister(rf, rd, l_byte),
               nmem, trap)
            end

        | PerformIType ((LBU, rs1, rd, immediate), (PC, rf, mem, trap))
          = let
              val (nmem, l_byte)
                  = MEM.LoadByteU(mem, Word32.+ (RF.LoadRegister(rf, rs1),
                                                 immediate));
            in
              (PC,
               RF.StoreRegister(rf, rd, l_byte),
               nmem, trap)
            end

        | PerformIType ((SB, rs1, rd, immediate), (PC, rf, mem, trap))
          = (PC,
             rf,
             MEM.StoreByte(mem,
                           Word32.+ (RF.LoadRegister(rf, rs1), immediate),
                           Word32.andb(0wx000000FF, RF.LoadRegister(rf, rd))),
             trap)

        | PerformIType ((LH, rs1, rd, immediate), (PC, rf, mem, trap))
          = let
              val (nmem, l_hword)
                  = MEM.LoadHWord(mem, Word32.+ (RF.LoadRegister(rf, rs1),
                                                 immediate));
            in
              (PC,
               RF.StoreRegister(rf, rd, l_hword),
               nmem, trap)
            end

        | PerformIType ((LHU, rs1, rd, immediate), (PC, rf, mem, trap))
          = let
              val (nmem, l_hword)
                  = MEM.LoadHWordU(mem, Word32.+ (RF.LoadRegister(rf, rs1),
                                                  immediate));
            in
              (PC,
               RF.StoreRegister(rf, rd, l_hword),
               nmem, trap)
            end

        | PerformIType ((SH, rs1, rd, immediate), (PC, rf, mem, trap))
          = (PC,
             rf,
             MEM.StoreByte(mem,
                           Word32.+ (RF.LoadRegister(rf, rs1), immediate),
                           Word32.andb(0wx0000FFFF, RF.LoadRegister(rf, rd))),
             trap)


        | PerformIType ((LW, rs1, rd, immediate), (PC, rf, mem, trap))
          = let
              val (nmem, l_word)
                  = MEM.LoadWord(mem, Word32.+ (RF.LoadRegister(rf, rs1),
                                                immediate));
            in
              (PC,
               RF.StoreRegister(rf, rd, l_word),
               nmem, trap)
            end

        | PerformIType ((SW, rs1, rd, immediate), (PC, rf, mem, trap))
          = (PC,
             rf,
             MEM.StoreWord(mem,
                           Word32.+ (RF.LoadRegister(rf, rs1), immediate),
                           RF.LoadRegister(rf, rd)),
             trap)

        | PerformIType ((_, rs1, rd, immediate), (PC, rf, mem, trap))
          = (print "Error : Non I-Type opcode, performing NOP\n";
             (PC, rf, mem, trap));


      (*
       * The function PerformRType performs one of the R-Type
       * instructions.  All of the instructions make use of the
       * ALU, and as such, call ALU.PerformAL.
       *)
      fun PerformRType ((SPECIA, rs1, rs2, rd, shamt, NOP), (PC, rf, mem, trap))
          = (PC, rf, mem, trap)

        | PerformRType ((SPECIAL, rs1, rs2, rd, shamt, SLL), (PC, rf, mem, trap))
          = (PC,
             RF.StoreRegister(rf, rd,
                              ALU.PerformAL(ALU.SLL,
                                            RF.LoadRegister(rf, rs1),
                                            RF.LoadRegister(rf, rs2))),
             mem, trap)

        | PerformRType ((SPECIAL, rs1, rs2, rd, shamt, SRL), (PC, rf, mem, trap))
          = (PC,
             RF.StoreRegister(rf, rd,
                              ALU.PerformAL(ALU.SRL,
                                            RF.LoadRegister(rf, rs1),
                                            RF.LoadRegister(rf, rs2))),
             mem, trap)

        | PerformRType ((SPECIAL, rs1, rs2, rd, shamt, SRA), (PC, rf, mem, trap))
          = (PC,
             RF.StoreRegister(rf, rd,
                              ALU.PerformAL(ALU.SRA,
                                            RF.LoadRegister(rf, rs1),
                                            RF.LoadRegister(rf, rs2))),
             mem, trap)

        | PerformRType ((SPECIAL, rs1, rs2, rd, shamt, ADD), (PC, rf, mem, trap))
          = (PC,
             RF.StoreRegister(rf, rd,
                              ALU.PerformAL(ALU.ADD,
                                            RF.LoadRegister(rf, rs1),
                                            RF.LoadRegister(rf, rs2))),
             mem, trap)

        | PerformRType ((SPECIAL, rs1, rs2, rd, shamt, ADDU), (PC, rf, mem, trap))
          = (PC,
             RF.StoreRegister(rf, rd,
                              ALU.PerformAL(ALU.ADDU,
                                            RF.LoadRegister(rf, rs1),
                                            RF.LoadRegister(rf, rs2))),
             mem, trap)

        | PerformRType ((SPECIAL, rs1, rs2, rd, shamt, SUB), (PC, rf, mem, trap))
          = (PC,
             RF.StoreRegister(rf, rd,
                              ALU.PerformAL(ALU.SUB,
                                            RF.LoadRegister(rf, rs1),
                                            RF.LoadRegister(rf, rs2))),
             mem, trap)

        | PerformRType ((SPECIAL, rs1, rs2, rd, shamt, SUBU), (PC, rf, mem, trap))
          = (PC,
             RF.StoreRegister(rf, rd,
                              ALU.PerformAL(ALU.SUBU,
                                            RF.LoadRegister(rf, rs1),
                                            RF.LoadRegister(rf, rs2))),
             mem, trap)

        | PerformRType ((SPECIAL, rs1, rs2, rd, shamt, AND), (PC, rf, mem, trap))
          = (PC,
             RF.StoreRegister(rf, rd,
                              ALU.PerformAL(ALU.AND,
                                            RF.LoadRegister(rf, rs1),
                                            RF.LoadRegister(rf, rs2))),
             mem, trap)

        | PerformRType ((SPECIAL, rs1, rs2, rd, shamt, OR), (PC, rf, mem, trap))
          = (PC,
             RF.StoreRegister(rf, rd,
                              ALU.PerformAL(ALU.OR,
                                            RF.LoadRegister(rf, rs1),
                                            RF.LoadRegister(rf, rs2))),
             mem, trap)

        | PerformRType ((SPECIAL, rs1, rs2, rd, shamt, XOR), (PC, rf, mem, trap))
          = (PC,
             RF.StoreRegister(rf, rd,
                              ALU.PerformAL(ALU.XOR,
                                            RF.LoadRegister(rf, rs1),
                                            RF.LoadRegister(rf, rs2))),
             mem, trap)

        | PerformRType ((SPECIAL, rs1, rs2, rd, shamt, SEQ), (PC, rf, mem, trap))
          = (PC,
             RF.StoreRegister(rf, rd,
                              ALU.PerformAL(ALU.SEQ,
                                            RF.LoadRegister(rf, rs1),
                                            RF.LoadRegister(rf, rs2))),
             mem, trap)

        | PerformRType ((SPECIAL, rs1, rs2, rd, shamt, SNE), (PC, rf, mem, trap))
          = (PC,
             RF.StoreRegister(rf, rd,
                              ALU.PerformAL(ALU.SNE,
                                            RF.LoadRegister(rf, rs1),
                                            RF.LoadRegister(rf, rs2))),
             mem, trap)

        | PerformRType ((SPECIAL, rs1, rs2, rd, shamt, SLT), (PC, rf, mem, trap))
          = (PC,
             RF.StoreRegister(rf, rd,
                              ALU.PerformAL(ALU.SLT,
                                            RF.LoadRegister(rf, rs1),
                                            RF.LoadRegister(rf, rs2))),
             mem, trap)

        | PerformRType ((SPECIAL, rs1, rs2, rd, shamt, SGT), (PC, rf, mem, trap))
          = (PC,
             RF.StoreRegister(rf, rd,
                              ALU.PerformAL(ALU.SGT,
                                            RF.LoadRegister(rf, rs1),
                                            RF.LoadRegister(rf, rs2))),
             mem, trap)

        | PerformRType ((SPECIAL, rs1, rs2, rd, shamt, SLE), (PC, rf, mem, trap))
          = (PC,
             RF.StoreRegister(rf, rd,
                              ALU.PerformAL(ALU.SLE,
                                            RF.LoadRegister(rf, rs1),
                                            RF.LoadRegister(rf, rs2))),
             mem, trap)

        | PerformRType ((SPECIAL, rs1, rs2, rd, shamt, SGE), (PC, rf, mem, trap))
          = (PC,
             RF.StoreRegister(rf, rd,
                              ALU.PerformAL(ALU.SGE,
                                            RF.LoadRegister(rf, rs1),
                                            RF.LoadRegister(rf, rs2))),
             mem, trap)

        | PerformRType ((_, rs1, rs2, rd, shamt, _), (PC, rf, mem, trap))
          = (print "Error : Non R-Type opcode, performing NOP\n";
             (PC, rf, mem, trap));


      (*
       * The function PerformJType performs one of the J-Type
       * instructions.
       *)
      fun PerformJType ((J, offset), (PC, rf, mem, trap))
          = (Word32.fromInt (Int.+ (Word32.toIntX PC,
                                           Word32.toIntX
                                           (Word32.<< (offset, 0wx0002)))),
             rf, mem, trap)

        | PerformJType ((JR, offset), (PC, rf, mem, trap))
          = (RF.LoadRegister(rf,
                             Word32.toInt(Word32.andb (Word32.>> (offset,
                                                                  0wx0015),
                                                       0wx0000001F :
                                                       Word32.word))),
             rf, mem, trap)

        | PerformJType ((JAL, offset), (PC, rf, mem, trap))
          = (Word32.fromInt (Int.+ (Word32.toIntX PC,
                                           Word32.toIntX
                                           (Word32.<< (offset, 0wx0002)))),
             RF.StoreRegister(rf, 31, PC),
             mem, trap)

        | PerformJType ((JALR, offset), (PC, rf, mem, trap))
          = (RF.LoadRegister(rf,
                             Word32.toInt (Word32.andb (Word32.>> (offset,
                                                                   0wx0015),
                                                        0wx0000001F :
                                                        Word32.word))),
             RF.StoreRegister(rf, 31, PC),
             mem, trap)

        | PerformJType ((TRAP, 0wx00000003 : Word32.word), (PC, rf, mem, trap))
          = let
              val {inputFn, outputFn, state} = trap
              val {input, state} = inputFn {state = state}
              val trap = {inputFn = inputFn,
                          outputFn = outputFn,
                          state = state}
            in
              (PC,
               RF.StoreRegister(rf, 14, Word32.fromInt input),
               mem,
               trap)
            end

        | PerformJType ((TRAP, 0wx00000004 : Word32.word), (PC, rf, mem, trap))
          = let
              val output = Word32.toIntX (RF.LoadRegister(rf, 14));
              val {inputFn, outputFn, state} = trap
              val {state} = outputFn {output = output, state = state}
              val trap = {inputFn = inputFn,
                          outputFn = outputFn,
                          state = state}
            in
              (PC, rf, mem, trap)
            end

        | PerformJType ((_, offset), (PC, rf, mem, trap))
          = (print "Error : Non J-Type opcode, performing NOP\n";
             (PC, rf, mem, trap));


      (*
       * The function PerformInstr performs an instruction by
       * passing the instruction to the appropriate auxiliary function.
       *)
      fun PerformInstr (ITYPE instr, (PC, rf, mem, trap))
          = PerformIType (instr, (PC, rf, mem, trap))
        | PerformInstr (RTYPE instr, (PC, rf, mem, trap))
          = PerformRType (instr, (PC, rf, mem, trap))
        | PerformInstr (JTYPE instr, (PC, rf, mem, trap))
          = PerformJType (instr, (PC, rf, mem, trap))
        | PerformInstr (ILLEGAL, (PC, rf, mem, trap))
          = (PC, rf, mem, trap);


      (*
       * The function CycleLoop represents the basic clock cycle of
       * the DLX processor.  It takes as input the current program
       * counter, the current register file, and the current memory.
       * It loads, decodes, and executes an instruction and increments
       * the program counter.  If the instruction that was loaded is
       * the HALT instruction, the program terminates, otherwise,
       * CycleLoop is recursively called with the result of performing
       * the instruction.
       *)
      fun CycleLoop (PC, rf, mem, trap)
          = let
              val (nmem, instr_word) = MEM.LoadWord (mem, PC);
              val instr = DecodeInstr instr_word;
              val nPC = Word32.+ (PC, 0wx00000004 : Word32.word);
            in
              if instr = HALT orelse instr = ILLEGAL
                then (fn () => MEM.GetStatistics nmem, #state trap)
                else CycleLoop (PerformInstr (instr, (nPC, rf, nmem, trap)))
            end


      (*
       * The function LoadProgAux is an auxilary function that
       * assists in loading a program into memory.  It recursively
       * calls itself, each time loading an instruction and incrementing
       * the address to which the next instruction is to be loaded.
       *)
      fun LoadProgAux ([], mem, address)
          = mem
        | LoadProgAux (instrs::instr_list, mem, address)
          = let
              val instro = Word32.fromString instrs;
              val instr = if isSome instro
                            then valOf instro
                            else (print ("Error : Invalid " ^
                                         "instruction format, " ^
                                         "returning NOP\n");
                                  0wx00000000 : Word32.word);
            in
              LoadProgAux (instr_list,
                           MEM.StoreWord (mem, address, instr),
                           Word32.+ (address, 0wx00000004 : Word32.word))
            end;

      (*
       * The function LoadProg takes a list of instructions and memory, and
       * loads the file into memory, beginning at 0x10000.
       *)
      fun LoadProg (instr_list, mem)
          = LoadProgAux (instr_list, mem, 0wx00010000 : Word32.word);


      (*
       * The function ReadFileToInstr reads the sequence of
       * instructions in a file into a list.
       *)
      fun ReadFileToInstr file
         = (case TextIO.inputLine file of
               NONE => []
             | SOME l => l :: (ReadFileToInstr file));


      (*
       * The function run_prog is exported by DLXSimulator.
       * It takes a list of instructions, then begins
       * execution of the instructions loaded at 0x10000, with an
       * initialized register file, and the loaded program in an
       * initialised memory.
       *)
      fun run_prog {instructions, trap}
          = let
               val (statistics, state)
                 = CycleLoop (0wx00010000 : Word32.word,
                              RF.InitRegisterFile (),
                              LoadProg (instructions, MEM.InitMemory ()),
                              trap);
            in
              {state = state,
               statistics = statistics}
            end

      (*
       * The function run_file is exported by DLXSimulator.
       * It takes the name of a file to be run, then begins
       * execution of the loaded program at 0x10000, with an
       * initialized register file, and the loaded program in an
       * initialized memory.
       *)
      fun run_file filename
          = let
              val instructions = ReadFileToInstr (TextIO.openIn filename)
              val trap = let
                           fun inputFn {state = ()} =
                             let
                               val x = TextIO.print "Value? ";
                               val input =
                                  case TextIO.inputLine TextIO.stdIn of
                                     NONE => (TextIO.print "Error : Returning 0\n";
                                              Int.fromInt 0)
                                   | SOME s =>
                                        (case Int.fromString s of
                                            NONE => (TextIO.print "Error : Returning 0\n";
                                                     Int.fromInt 0)
                                          | SOME i => i)
                             in
                               {input = input, state = ()}
                             end
                           fun outputFn {output, state = ()} =
                             (TextIO.print ("Output: " ^ (Int.toString output) ^ "\n")
                              ; {state = ()})
                         in
                           {inputFn = inputFn,
                            outputFn = outputFn,
                            state = ()}
                         end
              val {state, statistics} = run_prog {instructions = instructions, trap = trap}
            in
               print "Program halted.\n";
               print (statistics ());
               ()
            end

    end
