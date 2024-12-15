(* stringmap.sml
 *)

signature STRINGMAP =
  sig type 'a stringmap
      exception Stringmap
      val new : unit -> '1a stringmap
      val add : 'a stringmap -> string * 'a -> unit
      val rm  : 'a stringmap -> string -> unit
      val map : 'a stringmap -> string -> 'a
      val app : (string * 'a -> unit) -> 'a stringmap -> unit
      val isin : 'a stringmap -> string -> bool
      val extract : 'a stringmap -> 'a list
  end

structure Stringmap : STRINGMAP =
struct
  type 'a stringmap = (string * 'a) list array
  exception Stringmap
  val hashFactor = 32
  and tableSize = 2357

  val sub = Array.sub
  infix 9 sub

  fun ordof(s, i) = Char.ord(String.sub(s, i))

  (* a string hashing function
     returns a number between 0 and tableSize-1 *)
  fun hash(str: string) : int =
      let val nchars = String.size str

          fun loop(i,n,r) =
            if i < n then
             loop(i+1,n,(hashFactor * r + ordof(str,i)) mod tableSize)
            else r

       in loop(0,nchars,0)
(*        while !i < nchars do
              (n := (hashFactor * !n + ordof(str, !i)) mod tableSize;
               i := !i + 1);
          !n
*)
      end

  (* create a new stringmap *)
  fun new (): '1a stringmap = Array.array(tableSize,nil)

  (* add a mapping pair s +-> x to the stringmap a *)
  fun add a (s,x) =
    let val index = hash s
     in Array.update(a,index,(s,x)::(a sub index))
    end

  (* apply the stringmap a to the index string s *)
  fun map a s =
    let fun find ((s',x)::r) = if s=s' then x else find r
          | find nil = raise Stringmap
     in find (a sub (hash s))
    end

  (* return true if the string is in the map, false otherwise *)
  fun isin a s =
      ((map a s; true)
       handle Stringmap => false)

  (* remove all pairs mapping string s from stringmap a *)
  fun rm a s = let fun f ((b as (s',j))::r) =
                                if s=s' then f r else b :: f r
                      | f nil = nil
                    val index = hash s
                 in Array.update(a,index, f(a sub index))
                end

  (* apply a function f to all mapping pairs in stringmap a *)
  fun app (f: string * 'a -> unit) a =
      let fun zap 0 = ()
            | zap n = let val m = n-1 in List.app f (a sub m); zap m end
      in  zap tableSize
      end

  (* extract the stringmap items as a list *)
  fun extract a =
      let fun atol n =
          if n < Array.length a then (a sub n) :: atol (n + 1)
          else nil
          val al = atol 0
          val fal = List.concat al
          fun strip (s, v) = v
          val answer = List.map strip fal
      in
          answer
      end

end  (* Stringmap *)
