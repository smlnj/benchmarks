(* memory.sml
 *
 * This defines the Memory structure, which provides the functionality
 * of memory.  The datatype memory provides the encapsulation of
 * memory, InitMemory initializes memory, setting all
 * addresses to zero, LoadWord takes memory and
 * a Word32.word corresponding to the address, and returns the
 * Word32.word value at that address and the updated memory,
 * StoreWord takes memory, a Word32.word corresponding to the
 * address, and a Word32.word and returns memory updated with the word
 * stored at the appropriate address.  LoadHWord, LoadHWordU,
 * LoadByte, and LoadByteU load halfwords, unsigned halfwords,
 * bytes, and unsigned bytes respectively from memory into the
 * lower portion of the returned Word32.word.  StoreHWord and
 * StoreByte store halfwords and bytes taken from the lower portion
 * of the Word32.word into memory.
 * GetStatistics takes memory and returns the read and write
 * statistics as a string.
 *
 * The underlying structure of memory is an immutable array of Word32.word.
 * The array has a length of 0x10000, since every element of the array
 * corresponds to a thirty-two bit integer.
 *
 * Also, the functions AlignWAddress and AlignHWAddress aligns a memory
 * address to a word and halfword address, respectively.  If LoadWord,
 * StoreWord, LoadHWord, LoadHWordU, or StoreHWord is asked to access an
 * unaligned address, it writes an error message, and uses the address
 * rounded down to the aligned address.
 *)

structure Memory : MEMORY
  = struct

      type memory = Word32.word ImmArray.immarray * (int * int);

      fun InitMemory () =
        (ImmArray.immarray(Word32.toInt(0wx10000 : Word32.word),
                           0wx00000000 : Word32.word),
         (0, 0)) : memory;

      fun AlignWAddress address
          = Word32.<< (Word32.>> (address, 0wx0002), 0wx0002);

      fun AlignHWAddress address
          = Word32.<< (Word32.>> (address, 0wx0001), 0wx0001);

      (* Load and Store provide errorless access to memory.
       * They provide a common interface to memory, while
       * the LoadX and StoreX specifically access words,
       * halfwords and bytes, requiring address to be aligned.
       * In Load and Store, two intermediate values are
       * generated.  The value aligned_address is the aligned
       * version of the given address, and is used to compare with
       * the original address to determine if it was aligned.  The
       * value use_address is equivalent to aligned_address divided
       * by four, and it corresponds to the index of the memory
       * array where the corresponding aligned address can be found.
       *)

      fun Load ((mem, (reads, writes)), address)
          = let
              val aligned_address = AlignWAddress address;
              val use_address = Word32.>> (aligned_address, 0wx0002);
            in
              ((mem, (reads + 1, writes)),
               ImmArray.sub(mem, Word32.toInt(use_address)))
            end;

      fun Store ((mem, (reads, writes)), address, data)
          = let
              val aligned_address = AlignWAddress address;
              val use_address = Word32.>> (aligned_address, 0wx0002);
            in
              (ImmArray.update(mem, Word32.toInt(use_address), data),
               (reads, writes + 1))
            end;


      fun LoadWord (mem, address)
          = let
              val aligned_address
                  = if address = AlignWAddress address
                      then address
                      else (print "Error LW: Memory using aligned address\n";
                            AlignWAddress address);
            in
              Load(mem, aligned_address)
            end;

      fun StoreWord (mem, address, data)
          = let
              val aligned_address
                  = if address = AlignWAddress address
                      then address
                      else (print "Error SW: Memory using aligned address\n";
                            AlignWAddress address);
            in
              Store(mem, aligned_address, data)
            end;

      fun LoadHWord (mem, address)
          = let
              val aligned_address
                  = if address = AlignHWAddress address
                      then address
                      else (print "Error LH: Memory using aligned address\n";
                            AlignHWAddress address);
              val (nmem,l_word) = Load(mem, aligned_address);
            in
              (nmem,
               case aligned_address
                 of 0wx00000000 : Word32.word
                   => Word32.~>>(Word32.<<(l_word, 0wx0010),
                                 0wx0010)
                  | 0wx00000010 : Word32.word
                   => Word32.~>>(Word32.<<(l_word, 0wx0000),
                                 0wx0010)
                  | _ => (print "Error LH: Memory returning 0\n";
                          0wx00000000 : Word32.word))
            end;

      fun LoadHWordU (mem, address)
          = let
              val aligned_address
                  = if address = AlignHWAddress address
                      then address
                      else (print "Error LHU: Memory using aligned address\n";
                            AlignHWAddress address);
              val (nmem, l_word) = Load(mem, aligned_address);
            in
              (nmem,
               case aligned_address
                 of 0wx00000000 : Word32.word
                   => Word32.>>(Word32.<<(l_word, 0wx0010),
                                0wx0010)
                  | 0wx00000010 : Word32.word
                   => Word32.>>(Word32.<<(l_word, 0wx0000),
                                0wx0010)
                  | _ => (print "Error LHU: Memory returning 0\n";
                          0wx00000000 : Word32.word))
            end;

      fun StoreHWord (mem, address, data)
          = let
              val aligned_address
                  = if address = AlignHWAddress address
                      then address
                      else (print "Error SH: Memory using aligned address\n";
                            AlignWAddress address);
              val (_, s_word) = Load(mem, aligned_address);
            in
              case aligned_address
                of 0wx00000000 : Word32.word
                  => Store(mem, aligned_address,
                           Word32.orb(Word32.andb(0wxFFFF0000 : Word32.word,
                                                  s_word),
                                      Word32.<<(Word32.andb(0wx0000FFFF :
                                                            Word32.word,
                                                            data),
                                                0wx0000)))
                 | 0wx00000010 : Word32.word
                  => Store(mem, aligned_address,
                           Word32.orb(Word32.andb(0wx0000FFFF : Word32.word,
                                                  s_word),
                                      Word32.<<(Word32.andb(0wx0000FFFF :
                                                            Word32.word,
                                                            data),
                                                0wx0010)))
                 | _ => (print "Error SH: Memory unchanged\n";
                         mem)
            end;

      fun LoadByte (mem, address)
          = let
              val aligned_address = address;
              val (nmem, l_word) = Load(mem, aligned_address);
            in
              (nmem,
               case aligned_address
                 of 0wx00000000 : Word32.word
                   => Word32.~>>(Word32.<<(l_word,
                                           0wx0018),
                                 0wx0018)
                  | 0wx00000008 : Word32.word
                   => Word32.~>>(Word32.<<(l_word,
                                           0wx0010),
                                 0wx0018)
                  | 0wx00000010 : Word32.word
                   => Word32.~>>(Word32.<<(l_word,
                                           0wx0008),
                                 0wx0018)
                  | 0wx00000018 : Word32.word
                   => Word32.~>>(Word32.<<(l_word,
                                           0wx0000),
                                 0wx0018)
                  | _ => (print "Error LB: Memory returning 0\n";
                          0wx00000000 : Word32.word))
            end;

      fun LoadByteU (mem, address)
          = let
              val aligned_address = address;
              val (nmem, l_word) = Load(mem, aligned_address);
            in
              (nmem,
               case aligned_address
                 of 0wx00000000 : Word32.word
                   => Word32.>>(Word32.<<(l_word,
                                          0wx0018),
                                0wx0018)
                  | 0wx00000008 : Word32.word
                   => Word32.>>(Word32.<<(l_word,
                                          0wx0010),
                                0wx0018)
                  | 0wx00000010 : Word32.word
                   => Word32.>>(Word32.<<(l_word,
                                          0wx0008),
                                0wx0018)
                  | 0wx00000018 : Word32.word
                   => Word32.>>(Word32.<<(l_word,
                                          0wx0000),
                                0wx0018)
                  | _ => (print "Error LBU: Memory returning 0\n";
                          0wx00000000 : Word32.word))
            end;

      fun StoreByte (mem, address, data)
          = let
              val aligned_address = address;
              val (_, s_word) = Load(mem, aligned_address);
            in
              case aligned_address
                of 0wx00000000 : Word32.word
                  => Store(mem, aligned_address,
                           Word32.orb(Word32.andb(0wxFFFFFF00 : Word32.word,
                                                  s_word),
                                      Word32.<<(Word32.andb(0wx000000FF :
                                                            Word32.word,
                                                            data),
                                                0wx0000)))
                 | 0wx00000008 : Word32.word
                  => Store(mem, aligned_address,
                           Word32.orb(Word32.andb(0wxFFFF00FF : Word32.word,
                                                  s_word),
                                      Word32.<<(Word32.andb(0wx000000FF :
                                                            Word32.word,
                                                            data),
                                                0wx0008)))
                 | 0wx00000010 : Word32.word
                  => Store(mem, aligned_address,
                           Word32.orb(Word32.andb(0wxFF00FFFF : Word32.word,
                                                  s_word),
                                      Word32.<<(Word32.andb(0wx000000FF :
                                                            Word32.word,
                                                            data),
                                                0wx0010)))
                 | 0wx00000018 : Word32.word
                  => Store(mem, aligned_address,
                           Word32.orb(Word32.andb(0wx00FFFFFF : Word32.word,
                                                  s_word),
                                      Word32.<<(Word32.andb(0wx000000FF :
                                                            Word32.word,
                                                            data),
                                                0wx0018)))
                 | _ => (print "Error SB: Memory unchanged\n";
                         mem)
            end;

      fun GetStatistics (mem, (reads, writes))
          = "Memory :\n" ^
            "Memory Reads : " ^ (Int.toString reads) ^ "\n" ^
            "Memory Writes : " ^ (Int.toString writes) ^ "\n";

    end
