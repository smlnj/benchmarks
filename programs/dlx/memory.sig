(* memory.sig
 *
 * This defines the exported datatype and functions provided by
 * memory.  The datatype memory provides the encapsulation
 * of memory, InitMemory initializes memory, setting all
 * addresses to zero, LoadWord takes memory and
 * a Word32.word corresponding to the address, and returns the
 * Word32.word value at that address, StoreWord takes memory,
 * a Word32.word corresponding to the address, and a
 * Word32.word and returns memory updated with the word
 * stored at the appropriate address.  LoadHWord, LoadHWordU,
 * LoadByte, and LoadByteU load halfwords, unsigned halfwords,
 * bytes, and unsigned bytes respectively from memory into the
 * lower portion of the returned Word32.word.  StoreHWord and
 * StoreByte store halfwords and bytes taken from the lower portion
 * of the Word32.word into memory.
 * GetStatistics takes memory and returns the read and write
 * statistics as a string.
 *)

signature MEMORY
  = sig

      type memory;

      val InitMemory : unit -> memory;

      val LoadWord : memory * Word32.word -> memory * Word32.word;
      val StoreWord : memory * Word32.word * Word32.word -> memory;

      val LoadHWord : memory * Word32.word -> memory * Word32.word;
      val LoadHWordU : memory * Word32.word -> memory * Word32.word;
      val StoreHWord : memory * Word32.word * Word32.word -> memory;

      val LoadByte : memory * Word32.word -> memory * Word32.word;
      val LoadByteU : memory * Word32.word -> memory * Word32.word;
      val StoreByte : memory * Word32.word * Word32.word -> memory;

      val GetStatistics : memory -> string;

    end
