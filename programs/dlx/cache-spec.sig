(* cache-spec.sig
 *
 * This defines the signature that outlines the specifications to
 * describe a cache.  The two datatypes are given to provide clear
 * means of differentiating between the write hit and write miss
 * options.  CacheName can be any string describing the cache.
 * CacheSize is an integer that represents the total number of words
 * in the cache.  BlockSize is an integer that represents the total
 * number of words in a block.  Associativity is an integer that
 * represents the associativity of the cache.  WriteHit and WriteMiss
 * represent the write hit and write miss options to be implemented by
 * this cache.
 *)

signature CACHESPEC
  = sig

      datatype WriteHitOption = Write_Through
                              | Write_Back;

      datatype WriteMissOption = Write_Allocate
                               | Write_No_Allocate;

      val CacheName : string;
      val CacheSize : int;
      val BlockSize : int;
      val Associativity : int;
      val WriteHit : WriteHitOption;
      val WriteMiss : WriteMissOption;

    end;
