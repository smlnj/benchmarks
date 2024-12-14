(* l1-cache1.sml
 *)

structure L1Cache1 : MEMORY
  = CachedMemory (structure CS = L1CacheSpec1
                  structure MEM = Memory )
