(* register-file.sml
 *
 * This defines the RegisterFile structure, which provides the
 * functionality of the register file.  The datatype registerfile
 * provides the encapsulation of the register file, InitRegisterFile
 * initializes the registerfile, setting all registers to zero and
 * setting r0, gp, sp, and fp to their appropriate values,
 * LoadRegister takes a registerfile and an integer corresponding to
 * the register, and returns the Word32.word value at that register,
 * and StoreRegister takes a registerfile, an integer corresponding to
 * the register, and a Word32.word and returns the registerfile
 * updated with the word stored in the appropriate register.
 *
 * The underlying structure of registerfile is an immutable array of
 * Word32.word.
 *)

structure RegisterFile : REGISTERFILE
  = struct

      type registerfile = Word32.word ImmArray.immarray;

      fun InitRegisterFile ()
          = ImmArray.update
            (ImmArray.update
             (ImmArray.update
              (ImmArray.update
               (ImmArray.immarray(32, 0wx00000000 : Word32.word),
                00, 0wx00000000 : Word32.word),
               28, 0wx00000000 : Word32.word),
              29, 0wx00040000 : Word32.word),
             30, 0wx00040000 : Word32.word) : registerfile;

      fun LoadRegister (rf, reg) = ImmArray.sub(rf, reg);

      fun StoreRegister (rf, reg, data) = ImmArray.update(rf, reg, data);

    end;
