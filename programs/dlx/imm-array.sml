(* imm-array.sml
 *
 * The ImmArray structure defines an immutable array implementation.
 * An immarray is stored internally as a list.
 * This results in O(n) sub and update functions, as opposed
 * to O(1) sub and update functions found in Array.  However,
 * immutable arrays are truly immutable, and can be integrated
 * with a functionally programming style easier than mutable
 * arrays.
 *
 * The ImmArray structure mimics the Array structure as much as possible.
 * The most obvious deviation is that unit return types in Array are replaced
 * by 'a immarray return types in ImmArray.  Unlike an 'a array, an 'a immarray
 * is an equality type if and only if 'a is an equality type.  Further immarray
 * equality is structural, rather than the "creation" equality used by Array.
 * Additionally, no vector type is supported, and consequently no copyVec
 * function is supported.  Finally, the functions mapi and map provide
 * similar functionality as modifyi and modify, but relax the constraint that
 * the argument function need be of type 'a -> 'a.
 *
 * Future Work : There are random-access list implementations
 *               that support O(log n) sub and update functions,
 *               which may provide a faster implementation, although
 *               possibly at the expense of space and the ease of
 *               implementing app, foldl, foldr, modify, and map functions.
 *)

signature IMMARRAY
  = sig
      type 'a immarray;

      val maxLen : int;
      val immarray : (int * 'a) -> 'a immarray;
      val fromList : 'a list -> 'a immarray;
      val toList : 'a immarray -> 'a list;

      val tabulate : int * (int -> 'a) -> 'a immarray;
      val length : 'a immarray -> int;

      val sub : 'a immarray * int -> 'a;
      val update : 'a immarray * int * 'a -> 'a immarray;
      val extract : 'a immarray * int * int option -> 'a immarray;

      val copy : {src : 'a immarray, si : int, len : int option,
                  dst : 'a immarray, di : int} -> 'a immarray;

      val appi : (int * 'a -> unit) -> ('a immarray * int * int option)
                 -> unit;
      val app : ('a -> unit) -> 'a immarray -> unit;
      val foldli : ((int * 'a * 'b) -> 'b) -> 'b
                   -> ('a immarray * int * int option) -> 'b;
      val foldri : ((int * 'a * 'b) -> 'b) -> 'b
                   -> ('a immarray * int * int option) -> 'b;
      val foldl : (('a * 'b) -> 'b) -> 'b -> 'a immarray -> 'b;
      val foldr : (('a * 'b) -> 'b) -> 'b -> 'a immarray -> 'b;
      val mapi : ((int * 'a) -> 'b) -> ('a immarray * int * int option)
                 ->  'b immarray;
      val map : ('a -> 'b) -> 'a immarray -> 'b immarray;
      val modifyi : ((int * 'a) -> 'a) -> ('a immarray * int * int option)
                    -> 'a immarray;
      val modify : ('a -> 'a) -> 'a immarray -> 'a immarray;
    end;


structure ImmArray : IMMARRAY
  = struct

      (* datatype 'a immarray
       * An immarray is stored internally as a list.
       * The use of a constructor prevents list functions from
       * treating immarray type as a list.
       *)
      datatype 'a immarray = IA of 'a list;

      (* val maxLen : int
       * The maximum length of immarrays supported.
       * Technically, under this implementation, the maximum length
       * of immarrays is the same as the maximum length of a list,
       * but for convience and compatibility, use the Array structure's
       * maximum length.
       *)
      val maxLen = Array.maxLen;

      (* val tabulate : int * (int -> 'a) -> 'a immarray
       * val immarray : int * 'a -> 'a immarray
       * val fromList : 'a list -> 'a immarray
       * val toList : 'a immarray -> 'a list
       * val length : 'a immarray -> int
       * These functions perform basic immarray functions.
       * The tabulate, immarray, and fromList functions create an immarray.
       * The toList function converts an immarray to a list.
       * The length function returns the length of an immarray.
       *)
      fun tabulate (n, initfn) = IA (List.tabulate (n, initfn));
      fun immarray (n, init) = tabulate (n, fn _ => init);
      fun fromList l = IA l;
      fun toList (IA ia) = ia;
      fun length (IA ia) = List.length ia;

      (* val sub : 'a immarray * int -> 'a
       * val update : 'a immarray * int * 'a -> 'a immarray
       * These functions sub and update an immarray by index.
       *)
      fun sub (IA ia, i) = List.nth (ia, i);
      fun update (IA ia, i, x) = IA ((List.take (ia, i)) @
                                     (x::(List.drop (ia, i + 1))));

      (* val extract : 'a immarray * int * int option -> 'a immarray
       * This function extracts an immarray slice from an immarray from
       * one index either through the rest of the immarray (NONE)
       * or for n elements (SOME n), as described in the
       * Standard ML Basis Library.
       *)
      fun extract (IA ia, i, NONE) = IA (List.drop (ia, i))
        | extract (IA ia, i, SOME n) = IA (List.take (List.drop (ia, i), n));

      (* val copy : {src : 'a immarray, si : int, len : int option,
                     dst : 'a immarray, di : int} -> 'a immarray
       * This function copies an immarray slice from src into dst starting
       * at the di element.
       *)
      fun copy {src, si, len, dst=IA ia, di}
        = let
            val IA sia = extract (src, si, len);
            val pre = List.take (ia, di);
            val post = case len
                         of NONE => List.drop (ia, di+(List.length sia))
                          | SOME n => List.drop (ia, di+n);
          in
            IA (pre @ sia @ post)
          end;

      (* val appi : ('a * int -> unit) -> ('a immarray * int * int option)
       *            -> unit
       * val app : ('a -> unit) -> 'a immarray -> unit
       * These functions apply a function to every element
       * of an immarray.  The appi function also provides the
       * index of the element as an argument to the applied function
       * and uses an immarray slice argument.
       *)
      local
        fun appi_aux f i [] = ()
          | appi_aux f i (h::t) = (f(i,h); appi_aux f (i + 1) t);
      in
        fun appi f (IA ia, i, len) = let
                                       val IA sia = extract (IA ia, i, len);
                                     in
                                       appi_aux f i sia
                                     end;
      end;
      fun app f immarr = appi (f o #2) (immarr, 0, NONE);

      (* val foldli : (int * 'a * 'b -> 'b) -> 'b
       *              -> ('a immarray * int * int option) -> 'b;
       * val foldri : (int * 'a * 'b -> 'b) -> 'b
       *              -> ('a immarray * int * int option) -> 'b;
       * val foldl : ('a * 'b -> 'b) -> 'b -> 'a immarray -> 'b
       * val foldr : ('a * 'b -> 'b) -> 'b -> 'a immarray -> 'b
       * These functions fold a function over every element
       * of an immarray.  The foldri and foldli functions also provide
       * the index of the element as an argument to the folded function
       * and uses an immarray slice argument.
       *)
      local
        fun foldli_aux f b i [] = b
          | foldli_aux f b i (h::t) = foldli_aux f (f(i,h,b)) (i+1) t;
        fun foldri_aux f b i [] = b
          | foldri_aux f b i (h::t) = f(i,h,foldri_aux f b (i+1) t);
      in
        fun foldli f b (IA ia, i, len)
          = let
              val IA ia2 = extract (IA ia, i, len);
            in
              foldli_aux f b i ia2
            end;
        fun foldri f b (IA ia, i, len)
          = let
              val IA ia2 = extract (IA ia, i, len);
            in
              foldri_aux f b i ia2
            end;
      end;
      fun foldl f b (IA ia) = foldli (fn (_,i,x) => f(i,x)) b (IA ia, 0, NONE);
      fun foldr f b (IA ia) = foldri (fn (_,i,x) => f(i,x)) b (IA ia, 0, NONE);

      (* val mapi : ('a * int -> 'b) -> 'a immarray -> 'b immarray
       * val map : ('a -> 'b) -> 'a immarray -> 'b immarray
       * These functions map a function over every element
       * of an immarray.  The mapi function also provides the
       * index of the element as an argument to the mapped function
       * and uses an immarray slice argument.  Although there are
       * similarities between mapi and modifyi, note that when mapi is
       * used with an immarray slice, the resulting immarray is the
       * same size as the slice.  This is necessary to preserve the
       * type of the resulting immarray.  Thus, mapi with the identity
       * function reduces to the extract function.
       *)
      local
        fun mapi_aux f i [] = []
          | mapi_aux f i (h::t) = (f (i,h))::(mapi_aux f (i + 1) t);
      in
        fun mapi f (IA ia, i, len) = let
                                       val IA ia2 = extract (IA ia, i, len);
                                     in
                                       IA (mapi_aux f i ia2)
                                     end;
      end;
      fun map f (IA ia)= mapi (f o #2) (IA ia, 0, NONE);

      (* val modifyi : (int * 'a -> 'a) -> ('a immarray * int * int option)
       *               -> 'a immarray
       * val modify : ('a -> 'a) -> 'a immarray -> 'a immarray
       * These functions apply a function to every element of an immarray
       * in left to right order and returns a new immarray where corresponding
       * elements are replaced by their modified values.  The modifyi
       * function also provides the index of the element as an argument
       * to the mapped function and uses an immarray slice argument.
       *)
      local
        fun modifyi_aux f i [] = []
          | modifyi_aux f i (h::t) = (f (i,h))::(modifyi_aux f (i + 1) t);
      in
        fun modifyi f (IA ia, i, len)
          = let
              val pre = List.take (ia, i);
              val IA ia2 = extract (IA ia, i, len);
              val post = case len
                           of NONE => []
                            | SOME n => List.drop (ia, i+n);
            in
              IA (pre @ (modifyi_aux f i ia2) @ post)
            end;
      end;
      fun modify f (IA ia) = modifyi (f o #2) (IA ia, 0, NONE);

    end;
