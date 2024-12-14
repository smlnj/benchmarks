(* l1-cache-spec1.sml
 *
 * This file describes a small simple level 1 cache.
 *)

structure L1CacheSpec1 : CACHESPEC
  = struct

      datatype WriteHitOption = Write_Through
                              | Write_Back;

      datatype WriteMissOption = Write_Allocate
                               | Write_No_Allocate;

      val CacheName = "Level 1 Cache";
      val CacheSize = 256;
      val BlockSize = 4;
      val Associativity = 2;
      val WriteHit = Write_Through;
      val WriteMiss = Write_No_Allocate;

    end;
