(* cached-memory.fun
 *
 * This defines the CachedMemory functor, which provides the
 * functionality of a cached memory and which takes two structures,
 * corresponding to the cache specification and the the level of
 * memory which the cache will be caching.  The datatype memory
 * provides the encapsulation of the cache along with the memory
 * system that is being cached, InitMemory initializes the cache and
 * the memory system that is being cached, LoadWord takes memory and a
 * Word32.word corresponding to the address, and returns the
 * Word32.word at that address and the updated cache and memory,
 * StoreWord takes memory, a Word32.word corresponding to the address,
 * and a Word32.word and returns the cache and memory updated with the
 * stored at the appropriate address.  LoadHWord, LoadHWordU,
 * LoadByte, and LoadByteU load halfwords, unsigned halfwords,
 * bytes, and unsigned bytes respectively from memory into the
 * lower portion of the returned Word32.word.  StoreHWord and
 * StoreByte store halfwords and bytes taken from the lower portion
 * of the Word32.word into memory.
 * GetStatistics takes memory and returns the read and write
 * statistics as a string.
 *
 * The underlying structure of cache is a two dimensional array of
 * cache lines, where a cache line consists of a valid bit, dirty bit,
 * a tag and a block of words, as a Word32.word array.
 * The size of the cache, the associativity, and the block size are
 * specified by the cache specification.
 *
 * Also, the functions AlignWAddress and AlignHWAddress aligns a memory
 * address to a word and halfword address, respectively.  If LoadWord,
 * StoreWord, LoadHWord, LoadHWordU, or StoreHWord is asked to access an
 * unaligned address, it writes an error message, and uses the address
 * rounded down to the aligned address.
 *)

functor CachedMemory (structure CS : CACHESPEC;
                      structure MEM : MEMORY;) : MEMORY
  = struct

      type cacheline
           = bool * bool * Word32.word * Word32.word ImmArray.immarray;

      type cacheset
           = cacheline ImmArray.immarray;

      type cache
           = cacheset ImmArray.immarray;

      type memory = (cache * (int * int * int * int)) * MEM.memory;


      (* Performs log[base2] on an integer. *)
      fun exp2 0 = 1
        | exp2 n = 2 * (exp2 (n-1))
      fun log2 x = let
                     fun log2_aux n = if exp2 n > x
                                        then (n-1)
                                        else log2_aux (n+1)
                   in
                     log2_aux 0
                   end

      open CS;

      (*
       * The following values of index size and field bits are
       * calculated from the values in the cache specification
       * structure.
       *)
      val IndexSize = CacheSize div (BlockSize * Associativity);
      val BlockOffsetBits = log2 (BlockSize * 4);
      val IndexBits = log2 IndexSize;
      val TagBits = 32 - BlockOffsetBits - IndexBits;


      (*
       * RandEntry returns a random number between
       * [0, Associativity - 1].  It is used to determine
       * replacement of data in the cache.
       *)
      val RandEntry = let
                        val modulus = Word.fromInt(Associativity - 1)
                      in
                        fn () => Word.toInt(Word.mod(Rand.rand (),
                                                     modulus))
                      end

      (*
       * The InitCache function initializes the cache to
       * not-valid, not-dirty, 0wx00000000 tag, blocks initialized
       * to 0wx00000000.
       *)
      fun InitCache ()
          = let
              val cacheline = (false, false, 0wx00000000 : Word32.word,
                               ImmArray.immarray (BlockSize,
                                                  0wx00000000 : Word32.word));
              val cacheset = ImmArray.immarray (Associativity, cacheline);
            in
              (ImmArray.immarray (IndexSize, cacheset),
               (0, 0, 0, 0))
            end;


      (*
       * The InitMemory function initializes the cache
       * and the memory being cached.
       *)
      fun InitMemory () = (InitCache (), MEM.InitMemory ()) : memory;


      (*
       * GetTag returns the Word32.word corresponding to the tag field of
       * address
       *)
      fun GetTag address
          = Word32.>> (address,
                       Word.fromInt (IndexBits + BlockOffsetBits));


      (*
       * GetIndex returns the Word32.word corresponding to the index
       * field of address.
       *)
      fun GetIndex address
          = let
              val mask
                  = Word32.notb
                    (Word32.<<
                     (Word32.>> (0wxFFFFFFFF : Word32.word,
                                 Word.fromInt (IndexBits + BlockOffsetBits)),
                      Word.fromInt (IndexBits + BlockOffsetBits)));
            in
              Word32.>> (Word32.andb (address, mask),
                         Word.fromInt (BlockOffsetBits))
            end;


      (*
       * GetBlockOffset returns the Word32.word corresponding to the
       * block offset field of address.
       *)
      fun GetBlockOffset address
          = let
              val mask
                  = Word32.notb
                    (Word32.<<
                     (Word32.>> (0wxFFFFFFFF : Word32.word,
                                 Word.fromInt BlockOffsetBits),
                      Word.fromInt BlockOffsetBits));
            in
              Word32.andb (address, mask)
            end;


      (*
       * The InCache* family of functions returns a boolean value
       * that determines if the word specified by address is in the
       * cache at the current time (and that the data is valid).
       *)
      fun InCache_aux_entry ((valid, dirty, tag, block), address)
          = tag = (GetTag address) andalso valid;

      fun InCache_aux_set (set, address)
          = ImmArray.foldr (fn (entry, result) =>
                               (InCache_aux_entry (entry, address)) orelse
                               result)
                           false
                           set;

      fun InCache (cac, address)
          = InCache_aux_set (ImmArray.sub (cac,
                                           Word32.toInt (GetIndex address)),
                             address);

      (*
       * The ReadCache* family of functions returns the Word32.word
       * stored at address in the cache.
       *)
      fun ReadCache_aux_entry ((valid, dirty, tag, block), address)
          = ImmArray.sub (block,
                          Word32.toInt (Word32.>> (GetBlockOffset address,
                                                   0wx0002)));

      fun ReadCache_aux_set (set, address)
          = ImmArray.foldr (fn (entry, result) =>
                               if InCache_aux_entry (entry, address)
                                 then ReadCache_aux_entry (entry, address)
                                 else result)
                           (0wx00000000 : Word32.word)
                           set;

      fun ReadCache (cac, address)
          = ReadCache_aux_set (ImmArray.sub (cac,
                                             Word32.toInt(GetIndex address)),
                               address);


      (*
       * The WriteCache* family of functions returns the updated
       * cache with data stored at address.
       *)
      fun WriteCache_aux_entry ((valid, dirty, tag, block), address, data)
          = let
              val ndirty = case WriteHit
                             of Write_Through => false
                              | Write_Back => true;
            in
              (true, ndirty, tag,
               ImmArray.update (block,
                                Word32.toInt (Word32.>>
                                              (GetBlockOffset address,
                                               0wx0002)),
                                data))
            end;

      fun WriteCache_aux_set (set, address, data)
          = ImmArray.map (fn entry =>
                             if InCache_aux_entry (entry, address)
                               then WriteCache_aux_entry (entry, address,
                                                          data)
                               else entry)
                         set;

      fun WriteCache (cac, address, data)
          = let
              val index = Word32.toInt (GetIndex address);
              val nset = WriteCache_aux_set (ImmArray.sub (cac, index),
                                             address, data);
            in
              ImmArray.update (cac, index, nset)
            end;


      (*
       * The LoadBlock function returns the updated
       * memory and the block containing address loaded from memory.
       *)
      fun LoadBlock (mem, address)
          = ImmArray.foldr (fn (offset, (block, mem)) =>
                               let
                                 val laddress
                                     = Word32.+ (Word32.<<
                                                 (Word32.>>
                                                  (address,
                                                   Word.fromInt
                                                   BlockOffsetBits),
                                                  Word.fromInt
                                                  BlockOffsetBits),
                                                 Word32.<< (Word32.fromInt
                                                            offset,
                                                            0wx0002));
                                 val (nmem, nword) = MEM.LoadWord (mem,
                                                                   laddress);
                               in
                                 (ImmArray.update (block, offset, nword), nmem)
                               end)
                           (ImmArray.immarray (BlockSize,
                                               0wx00000000 : Word32.word), mem)
                           (ImmArray.tabulate (BlockSize, fn i => i));


      (*
       * The StoreBlock functionsreturns the updated
       * memory with block stored into the block containing address.
       *)
      fun StoreBlock (block, mem, address)
          = ImmArray.foldr (fn (offset, mem) =>
                               let
                                 val saddress
                                     = Word32.+ (Word32.<<
                                                 (Word32.>>
                                                  (address,
                                                   Word.fromInt
                                                   BlockOffsetBits),
                                                  Word.fromInt
                                                  BlockOffsetBits),
                                                 Word32.<< (Word32.fromInt
                                                            offset,
                                                            0wx0002));
                               in
                                 MEM.StoreWord (mem, saddress,
                                                ImmArray.sub (block, offset))
                               end)
                           mem
                           (ImmArray.tabulate (BlockSize, fn i => i));


      (*
       * The LoadCache* family of functions returns the updated
       * cache and memory, with the block containing address loaded
       * into the cache at the appropriate cache line, and dirty
       * data written back to memory as needed.
       *)
      fun LoadCache_aux_entry ((valid, dirty, tag, block), mem, address)
          = let
              val saddress
                  = Word32.orb (Word32.<< (tag,
                                           Word.fromInt TagBits),
                                Word32.<< (GetIndex address,
                                           Word.fromInt IndexBits));
              val nmem = if valid andalso dirty
                           then StoreBlock (block, mem, saddress)
                           else mem;
              val (nblock, nnmem) = LoadBlock (nmem, address);
            in
              ((true, false, GetTag address, nblock), nnmem)
            end;

      fun LoadCache_aux_set (set, mem, address)
          = let
              val entry = RandEntry ();
              val (nentry, nmem) = LoadCache_aux_entry (ImmArray.sub (set,
                                                                      entry),
                                                        mem, address);
            in
              (ImmArray.update (set, entry, nentry), nmem)
            end;

      fun LoadCache (cac, mem, address)
          = let
              val index = Word32.toInt (GetIndex address);
              val (nset, nmem)
                  = LoadCache_aux_set (ImmArray.sub (cac, index),
                                       mem, address);
            in
              (ImmArray.update (cac, index, nset), nmem)
            end;


      (*
       * The remainder of the function defined here satisfy the MEMORY
       * signature.  This allows a CachedMemory to act exactly like
       * a normal Memory, and thus caches can be nested to an arbitrary
       * depth.
       *)

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

      fun Load (((cac, (rh, rm, wh, wm)), mem), address)
          = let
              val aligned_address = AlignWAddress address;
            in
              if InCache (cac, aligned_address)
                then (((cac, (rh + 1, rm, wh, wm)), mem),
                      ReadCache (cac, aligned_address))
                else let
                       val (ncac, nmem)
                           = LoadCache (cac, mem, aligned_address);
                     in
                       (((ncac, (rh, rm + 1, wh, wm)), nmem),
                        ReadCache (ncac, aligned_address))
                     end
            end;


      fun Store (((cac, (rh, rm, wh, wm)), mem), address, data)
          = let
              val aligned_address = AlignWAddress address;
            in
              if InCache (cac, aligned_address)
                then let
                       val ncac = WriteCache (cac, aligned_address, data);
                     in
                       case WriteHit
                         of Write_Through =>
                              ((ncac, (rh, rm, wh + 1, wm)),
                               MEM.StoreWord (mem, aligned_address, data))
                          | Write_Back =>
                              ((ncac, (rh, rm, wh + 1, wm)), mem)
                     end
                else case WriteMiss
                       of Write_Allocate =>
                            let
                              val (ncac, nmem)
                                = LoadCache (cac, mem, aligned_address);
                              val nncac
                                = WriteCache (ncac, aligned_address, data);
                            in
                              case WriteHit
                                of Write_Through =>
                                     ((nncac, (rh, rm, wh, wm + 1)),
                                      MEM.StoreWord (nmem, aligned_address,
                                                     data))
                                 | Write_Back =>
                                     ((nncac, (rh, rm, wh, wm + 1)),
                                      nmem)
                            end
                        | Write_No_Allocate =>
                            ((cac, (rh, rm, wh, wm + 1)),
                             MEM.StoreWord (mem, aligned_address, data))
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

      fun GetStatistics ((cac, (rh, rm, wh, wm)), mem)
          = let

              val th = rh + wh;

              val tm = rm + wm;

              val who = case WriteHit
                          of Write_Through => "Write Through"
                           | Write_Back => "Write Back";

              val wmo = case WriteMiss
                          of Write_Allocate => "Write Allocate"
                           | Write_No_Allocate => "Write No Allocate";

            in
              CacheName ^ " :\n" ^
              "CacheSize : " ^ (Int.toString CacheSize) ^ "\n" ^
              "BlockSize : " ^ (Int.toString BlockSize) ^ "\n" ^
              "Associativity : " ^ (Int.toString Associativity) ^ "\n" ^
              "Write Hit : " ^ who ^ "\n" ^
              "Write Miss : " ^ wmo ^ "\n" ^
              "Read hits : " ^ (Int.toString rh) ^ "\n" ^
              "Read misses : " ^ (Int.toString rm) ^ "\n" ^
              "Write hits : " ^ (Int.toString wh) ^ "\n" ^
              "Write misses : " ^ (Int.toString wm) ^ "\n" ^
              "Total hits : " ^ (Int.toString th) ^ "\n" ^
              "Total misses : " ^ (Int.toString tm) ^ "\n" ^
              (MEM.GetStatistics mem)
            end;

    end
