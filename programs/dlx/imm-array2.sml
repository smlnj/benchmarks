(* imm-array2.sml
 *
 * The ImmArray2 structure defines a two dimensional immutable array
 * implementation.  An immarray2 is stored internally as an immutable
 * array of immutable arrays.  As such, the ImmArray2 makes heavy use
 * of the ImmArray structure.
 *
 * The ImmArray2 structure mimics the Array2 structure as much as possible.
 * The most obvious deviation is that unit return types in Array2 are replaced
 * by 'a immarray2 return types in ImmArray2.  Unlike an 'a array,
 * an 'a immarray2 is an equality type if and only if 'a is an equality type.
 * Further immarray2 equality is structural, rather than the "creation"
 * equality used by Array2.  Also, the 'a region type is not included in
 * ImmArray2, but all functions in Array2 that require 'a regions are present
 * with arguments taken in the natural order.  Finally, the functions mapi
 * and map provide similar functionality as modifyi and modify, but relax
 * the constraint that the argument function need be of type 'a -> 'a.
 *)

signature IMMARRAY2
  = sig

      type 'a immarray2;

      datatype traversal = RowMajor | ColMajor

      val immarray2 : int * int * 'a -> 'a immarray2;
      val tabulate : traversal -> int * int * ((int * int) -> 'a)
                     -> 'a immarray2;
      val fromList : 'a list list -> 'a immarray2;
      val dimensions : 'a immarray2 -> int * int;

      val sub : 'a immarray2 * int * int -> 'a;
      val update : 'a immarray2 * int * int * 'a -> 'a immarray2;
      val extract : 'a immarray2 * int * int * int option * int option
                    -> 'a immarray2;

      val copy : {src : 'a immarray2, si : int, sj : int,
                  ilen : int option, jlen : int option,
                  dst : 'a immarray2, di : int, dj : int} -> 'a immarray2;

      val nRows : 'a immarray2 -> int;
      val nCols : 'a immarray2 -> int;
      val row : 'a immarray2 * int -> 'a ImmArray.immarray;
      val column : 'a immarray2 * int -> 'a ImmArray.immarray;

      val appi : traversal -> (int * int * 'a -> unit)
                 -> ('a immarray2 * int * int * int option * int option)
                 -> unit;
      val app : traversal -> ('a -> unit) -> 'a immarray2 -> unit;
      val foldli : traversal -> ((int * int * 'a * 'b) -> 'b) -> 'b
                   -> ('a immarray2 * int * int * int option * int option)
                   -> 'b
      val foldri : traversal -> ((int * int * 'a * 'b) -> 'b) -> 'b
                   -> ('a immarray2 * int * int * int option * int option)
                   -> 'b
      val foldl : traversal -> (('a * 'b) -> 'b) -> 'b -> 'a immarray2 -> 'b
      val foldr : traversal -> (('a * 'b) -> 'b) -> 'b -> 'a immarray2 -> 'b
      val mapi : traversal -> (int * int * 'a -> 'b)
                 -> ('a immarray2 * int * int * int option * int option)
                 -> 'b immarray2;
      val map : traversal -> ('a -> 'b) -> 'a immarray2 -> 'b immarray2;
      val modifyi : traversal -> ((int * int * 'a) -> 'a)
                    -> ('a immarray2 * int * int * int option * int option)
                    -> 'a immarray2;
      val modify : traversal -> ('a -> 'a) -> 'a immarray2 -> 'a immarray2;
    end

structure ImmArray2 : IMMARRAY2
  = struct

      (* datatype 'a immarray2
       * An immarray2 is stored internally as an immutable array
       * of immutable arrays.  The use of a contructor prevents ImmArray
       * functions from treating the immarray2 type as an immarray.
      *)
      datatype 'a immarray2 = IA2 of 'a ImmArray.immarray ImmArray.immarray;
      datatype traversal = RowMajor | ColMajor

      (* val tabulate : traversal -> int * int * (int * int -> 'a)
       *                -> 'a immarray2
       * val immarray2 : int * int * 'a -> 'a immarray2
       * val fromList : 'a list list -> 'a immarray2
       * val dmensions : 'a immarray2 -> int * int
       * These functions perform basic immarray2 functions.
       * The tabulate and immarray2 functions create an immarray2.
       * The fromList function converts a list of lists into an immarray2.
       * Unlike Array2.fromList, fromList will accept lists of different
       * lengths, allowing one to create an immarray2 in which the
       * rows have different numbers of columns, although it is likely that
       * exceptions will be raised when other ImmArray2 functions are applied
       * to such an immarray2.  Note that dimensions will return the
       * number of columns in row 0.
       * The dimensions function returns the dimensions of an immarray2.
       *)
      fun tabulate RowMajor (r, c, initfn)
        = let
            fun initrow r = ImmArray.tabulate (c, fn ic => initfn (r,ic));
          in
            IA2 (ImmArray.tabulate (r, fn ir => initrow ir))
          end
        | tabulate ColMajor (r, c, initfn)
          = turn (tabulate RowMajor (c,r, fn (c,r) => initfn(r,c)))
      and immarray2 (r, c, init) = tabulate RowMajor (r, c, fn (_, _) => init)
      and fromList l
        = IA2 (ImmArray.tabulate (length l,
                                  fn ir => ImmArray.fromList (List.nth(l,ir))))
      and dimensions (IA2 ia2) = (ImmArray.length ia2,
                                  ImmArray.length (ImmArray.sub (ia2, 0)))

      (* turn : 'a immarray2 -> 'a immarray2
       * This function reverses the rows and columns of an immarray2
       * to allow handling of ColMajor traversals.
       *)
      and turn ia2 = let
                       val (r,c) = dimensions ia2;
                     in
                       tabulate RowMajor (c,r,fn (cc,rr) => sub (ia2,rr,cc))
                     end

      (* val sub : 'a immarray2 * int * int -> 'a
       * val update : 'a immarray2 * int * int * 'a -> 'a immarray2
       * These functions sub and update an immarray2 by indices.
       *)
      and sub (IA2 ia2, r, c) = ImmArray.sub(ImmArray.sub (ia2, r), c);
      fun update (IA2 ia2, r, c, x)
          = IA2 (ImmArray.update (ia2, r,
                                  ImmArray.update (ImmArray.sub (ia2, r),
                                                   c, x)));

      (* val extract : 'a immarray2 * int * int *
       *               int option * int option -> 'a immarray2
       * This function extracts a subarray from an immarray2 from
       * one pair of indices either through the rest of the
       * immarray2 (NONE, NONE) or for the specfied number of elements.
       *)
      fun extract (IA2 ia2, i, j, rlen, clen)
          = IA2 (ImmArray.map (fn ia => ImmArray.extract (ia, j, clen))
                              (ImmArray.extract (ia2, i, rlen)));

      (* val nRows : 'a immarray2 -> int
       * val nCols : 'a immarray2 -> int
       * These functions return specific dimensions of an immarray2.
       *)
      fun nRows (IA2 ia2) = (#1 o dimensions) (IA2 ia2);
      fun nCols (IA2 ia2) = (#2 o dimensions) (IA2 ia2);
      (* val row : immarray2 * int -> ImmArray.immarray
       * val column : immarray2 * int -> ImmArray.immarray
       * These functions extract an entire row or column from
       * an immarray2 by index, returning the row or column as
       * an ImmArray.immarray.
       *)
      fun row (ia2, r) = let
                           val (c, _) = dimensions ia2;
                         in
                           ImmArray.tabulate (c, fn i => sub (ia2, r, i))
                         end;
      fun column (ia2, c) = let
                              val (_, r) = dimensions ia2;
                            in
                              ImmArray.tabulate (r, fn i => sub (ia2, i, c))
                            end;

      (* val copy : {src : 'a immarray2, si : int, sj : int,
       *             ilen : int option, jlen : int option,
       *             dst : 'a immarray2, di : int, dj : int};
       * This function copies an immarray2 slice from src int dst starting
       * at the di,dj element.
       *)
      fun copy {src, si, sj, ilen, jlen, dst=IA2 ia2, di, dj}
        = let
            val nilen = case ilen
                          of NONE => SOME ((nRows src) - si)
                           | SOME n => SOME n;
          in
            IA2 (ImmArray.modifyi (fn (r, ia)
                                   => ImmArray.copy {src=row (src, si+r-di),
                                                     si=sj, len=jlen,
                                                     dst=ia, di=dj})
                                  (ia2, di, nilen))
          end;

      (* val appi : traversal -> ('a * int * int -> unit) -> 'a immarray2
       *            -> unit
       * val app : traversal -> ('a -> unit) -> 'a immarray2 -> unit
       * These functions apply a function to every element
       * of an immarray2.  The appi function also provides the
       * indices of the element as an argument to the applied function
       * and uses an immarray2 slice argument.
       *)
      fun appi RowMajor f (IA2 ia2, i, j, rlen, clen)
        = ImmArray.appi (fn (r,ia) => ImmArray.appi (fn (c,x) => f(r,c,x))
                                                    (ia, j, clen))
                        (ia2, i, rlen)
        | appi ColMajor f (ia2, i, j, rlen, clen)
        = appi RowMajor (fn (c,r,x) => f(r,c,x)) (turn ia2, j, i, clen, rlen);
      fun app tr f (IA2 ia2) = appi tr (f o #3) (IA2 ia2, 0, 0, NONE, NONE);

      (* val foldli : traversal -> ((int * int * 'a * 'b) -> 'b) -> 'b
       *              -> ('a immarray2 * int * int * int option * int option)
       *              -> 'b
       * val foldri : traversal -> ((int * int * 'a * 'b) -> 'b) -> 'b
       *              -> ('a immarray2 * int * int * int option * int option)
       *              -> 'b
       * val foldl : traversal -> ('a * 'b -> 'b) -> 'b -> 'a immarray2 -> 'b
       * val foldr : traversal -> ('a * 'b -> 'b) -> 'b -> 'a immarray2 -> 'b
       * These functions fold a function over every element
       * of an immarray2.  The foldri and foldli functions also provide
       * the index of the element as an argument to the folded function
       * and uses an immarray2 slice argument.
       *)
      fun foldli RowMajor f b (IA2 ia2, i, j, rlen, clen)
        = ImmArray.foldli (fn (r,ia,b)
                           => ImmArray.foldli (fn (c,x,b) => f(r,c,x,b))
                                              b
                                              (ia, j, clen))
                          b
                          (ia2, i, rlen)
        | foldli ColMajor f b (ia2, i, j, rlen, clen)
        = foldli RowMajor (fn (c,r,x,b) => f(r,c,x,b)) b
                 (turn ia2, j, i, clen, rlen);
      fun foldri RowMajor f b (IA2 ia2, i, j, rlen, clen)
        = ImmArray.foldri (fn (r,ia,b)
                           => ImmArray.foldri (fn (c,x,b) => f(r,c,x,b))
                                              b
                                              (ia, j, clen))
                          b
                          (ia2, i, rlen)
        | foldri ColMajor f b (ia2, i, j, rlen, clen)
        = foldri RowMajor (fn (c,r,x,b) => f(r,c,x,b)) b
                          (turn ia2, j, i, clen, rlen);
      fun foldl tr f b (IA2 ia2)
        = foldli tr (fn (_,_,x,b) => f(x,b)) b (IA2 ia2, 0, 0, NONE, NONE);
      fun foldr tr f b (IA2 ia2)
        = foldri tr (fn (_,_,x,b) => f(x,b)) b (IA2 ia2, 0, 0, NONE, NONE);

      (* val mapi : traversal -> ('a * int * int -> 'b) -> 'a immarray2
       *            -> 'b immarray2
       * val map : traversal -> ('a -> 'b) -> 'a immarray2 -> 'b immarray2
       * These functions map a function over every element
       * of an immarray2.  The mapi function also provides the
       * indices of the element as an argument to the mapped function
       * and uses an immarray2 slice argument.  Although there are
       * similarities between mapi and modifyi, note that when mapi is
       * used with an immarray2 slice, the resulting immarray2 is the
       * same size as the slice.  This is necessary to preserve the
       * type of the resulting immarray2.  Thus, mapi with the identity
       * function reduces to the extract function.
       *)
      fun mapi RowMajor f (IA2 ia2, i, j, rlen, clen)
        = IA2 (ImmArray.mapi (fn (r,ia) => ImmArray.mapi (fn (c,x) => f(r,c,x))
                                                         (ia, j, clen))
                             (ia2, i, rlen))
        | mapi ColMajor f (ia2, i, j, rlen, clen)
        = turn (mapi RowMajor (fn (c,r,x) => f(r,c,x))
                     (turn ia2, j, i, clen, rlen))
      fun map tr f (IA2 ia2)
        = mapi tr (f o #3) (IA2 ia2, 0, 0, NONE, NONE);

      (* val modifyi : traversal -> (int * int* 'a -> 'a)
                       -> ('a immarray2 * int * int * int option * int option)
       *               -> 'a immarray2
       * val modify : traversal -> ('a -> 'a) -> 'a immarray2 -> 'a immarray2
       * These functions apply a function to every element of an immarray2
       * in row by column order and returns a new immarray2 where corresponding
       * elements are replaced by their modified values.  The modifyi
       * function also provides the index of the element as an argument
       * to the mapped function and uses an immarray2 slice argument.
       *)
      fun modifyi RowMajor f (IA2 ia2, i, j, rlen, clen)
        = IA2 (ImmArray.modifyi (fn (r,ia) => ImmArray.modifyi (fn (c,x)
                                                                => f(r,c,x))
                                                               (ia, j, clen))
              (ia2, i, rlen))
        | modifyi ColMajor f (ia2, i, j, rlen, clen)
        = turn (modifyi RowMajor (fn (c,r,x) => f(r,c,x))
               (turn ia2, j, i, clen, rlen));
      fun modify tr f (IA2 ia2)
        = modifyi tr (f o #3) (IA2 ia2, 0, 0, NONE, NONE);

    end
