(* register-file.sig
 *
 * This defines the exported datatype and functions provided by the
 * register file.  The datatype registerfile provides the encapsulation
 * of the register file, InitRegisterFile initializes the registerfile,
 * setting all registers to zero and setting r0, gp, sp, and fp to
 * their appropriate values, LoadRegister takes a registerfile and
 * an integer corresponding to the register, and returns the
 * Word32.word value at that register, and StoreRegister takes a
 * registerfile, an integer corresponding to the register, and a
 * Word32.word and returns the registerfile updated with the word
 * stored in the appropriate register.
 *)

signature REGISTERFILE
  = sig

      type registerfile;

      val InitRegisterFile : unit  -> registerfile;

      val LoadRegister : registerfile * int -> Word32.word;

      val StoreRegister : registerfile * int * Word32.word -> registerfile;

    end
