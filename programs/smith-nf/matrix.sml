(* matrix.sml
 *)

signature MATRIX =
   sig
      type 'entry matrix
      val make: int * int * (int * int -> 'entry) -> 'entry matrix
      val height: 'entry matrix -> int
      val width: 'entry matrix -> int
      val fetch: 'entry matrix * int * int -> 'entry
      val fetchRow: 'entry matrix * int -> int -> 'entry
      val fetchCol: 'entry matrix * int -> int -> 'entry
      val store: 'entry matrix * int * int * 'entry -> unit
      val storeRow: 'entry matrix * int -> int * 'entry -> unit
      val storeCol: 'entry matrix * int -> int * 'entry -> unit
      val rowSwap: 'entry matrix * int * int -> unit
      val colSwap: 'entry matrix * int * int -> unit
      val rowOp: 'entry matrix * int * int * ('entry * 'entry -> 'entry) -> unit
      val colOp: 'entry matrix * int * int * ('entry * 'entry -> 'entry) -> unit
      val copy: 'entry matrix -> 'entry matrix
      val map: 'entry1 matrix * ('entry1 -> 'entry2) -> 'entry2 matrix
      val toString: 'entry matrix * ('entry -> string) -> string
   end

structure Matrix:> MATRIX =
   struct
      type 'entry matrix = int * int * 'entry array

      exception sizeError

      exception index

      exception foldError

      fun make (height: int, width: int, generator: int * int -> 'entry)
         : 'entry matrix =
         if height < 0 orelse width < 0
            then raise sizeError
         else (height,
               width,
               Array.tabulate (height*width,
                               fn z => generator (z div width,
                                                  z mod width)))

      fun height (height, _, _) = height

      fun width (width, _, _) = width

      fun fetch ((height, width, mat), row, col) =
         if 0 <= row
            andalso row < height
            andalso 0 <= col
            andalso col < width
            then Array.sub (mat, col + width*row)
         else raise index

      fun fetchRow ((height, width, mat), row) =
         if 0 <= row andalso row < height
            then let val offset = width * row
                 in fn col =>
                    if 0 <= col andalso col < width
                       then Array.sub (mat, col + offset)
                    else raise index
                 end
         else raise index

      fun fetchCol ((height, width, mat), col) =
         if 0 <= col andalso col < width
            then fn row =>
               if 0 <= row andalso row < height
                  then Array.sub (mat, col + width*row)
               else raise index
         else raise index

      fun store ((height, width, mat), row, col, entry) =
         if 0 <= row
            andalso row < height
            andalso 0 <= col
            andalso col < width
            then Array.update (mat, col + width*row, entry)
         else raise index

      fun storeRow ((height, width, mat), row) =
         if 0 <= row andalso row < height
            then let val offset = width * row
                 in fn (col, entry) =>
                    if 0 <= col andalso col < width
                       then Array.update (mat, col + offset, entry)
                    else raise index
                 end
         else raise index

      fun storeCol ((height, width, mat), col) =
         if 0 <= col andalso col < width
            then fn (row, entry) =>
               if 0 <= row andalso row < height
                  then Array.update (mat, col + width*row, entry)
               else raise index
         else raise index

      fun swapLoop (from1: int -> 'entry,
                    to1: int * 'entry -> unit,
                    from2: int -> 'entry,
                    to2: int * 'entry -> unit,
                    limit: int): unit =
         let fun loop (i: int): unit =
            if i = limit
               then ()
            else let val tmp = from1 i
                 in to1 (i, from2 i);
                    to2 (i, tmp);
                    loop (i + 1)
                 end
         in loop 0
         end

      fun rowSwap (mat as (height, width, _), row1, row2): unit =
         if 0 <= row1 andalso row1 < height
            andalso 0 <= row2 andalso row2 < height
            then if row1 = row2
                    then ()
                 else swapLoop (fetchRow (mat, row1),
                                storeRow (mat, row1),
                                fetchRow (mat, row2),
                                storeRow (mat, row2),
                                width)
         else raise index

      fun colSwap (mat as (height, width, _), col1, col2): unit =
         if 0 <= col1 andalso col1 < width
            andalso 0 <= col2 andalso col2 < width
            then if col1 = col2
                    then ()
                 else swapLoop (fetchCol (mat, col1),
                                storeCol (mat, col1),
                                fetchCol (mat, col2),
                                storeCol (mat, col2),
                                height)
         else raise index

      fun opLoop (from1: int -> 'entry,
                  from2: int -> 'entry,
                  to2: int * 'entry -> unit,
                  limit: int,
                  f: 'entry * 'entry -> 'entry): unit =
         let fun loop (i: int): unit =
            if i = limit
               then ()
            else (
                  to2 (i,
                       f (from1 i, from2 i));
                  loop (i + 1))
         in loop 0
         end

      fun rowOp (mat as (height, width, _),
                 row1,
                 row2,
                 f: 'entry * 'entry -> 'entry): unit =
         if 0 <= row1 andalso row1 < height
            andalso 0 <= row2 andalso row2 < height
            andalso row1 <> row2
            then opLoop (fetchRow (mat, row1),
                         fetchRow (mat, row2),
                         storeRow (mat, row2),
                         width,
                         f)
         else raise index

      fun colOp (mat as (height, width, _),
                 col1,
                 col2,
                 f: 'entry * 'entry -> 'entry): unit =
         if 0 <= col1 andalso col1 < width
            andalso 0 <= col2 andalso col2 < width
            andalso col1 <> col2
            then opLoop (fetchCol (mat, col1),
                         fetchCol (mat, col2),
                         storeCol (mat, col2),
                         height,
                         f)
         else raise index

      fun copy ((height, width, mat)) =
         (height,
          width,
          Array.tabulate (Array.length mat,
                          fn i => Array.sub (mat, i)))

      fun map ((height, width, mat: 'entry1 Array.array),
               f: 'entry1 -> 'entry2)
         : 'entry2 matrix =
         (height,
          width,
          Array.tabulate (Array.length mat,
                          fn i => f (Array.sub (mat, i))))

      (* Natural fold a range of integers in reverse. *)
      fun naturalFold (limit: int,
                       state: 'state,
                       folder: int * 'state -> 'state): 'state =
         let fun loop (i: int, state: 'state) =
            if i = 0
               then state
            else loop (i - 1, folder (i - 1, state))
         in if limit < 0
               then raise foldError
            else loop (limit, state)
         end


      local val blank8 = Byte.charToByte #" "

         fun makeBlanks size =
            let val blanks = Word8Vector.tabulate (size,
                                                   fn _ => blank8)
            in Byte.bytesToString blanks
            end

      in fun toString (mat: 'entry matrix, f: 'entry -> string): string =
         let val mat as (height, width, _) = map (mat, f)
            fun maxSize from (i, width) = Int.max (String.size (from i),
                                                   width)
            fun colWidth col = naturalFold (height,
                                            0,
                                            maxSize (fetchCol (mat,
                                                               col)))
            val widths = Vector.tabulate (width, colWidth)
            fun doRow (row: int, ac: string list): string list =
               let val from = fetchRow (mat, row)
                  fun loop (col: int, ac: string list) =
                     let val next = from col
                        val ac = next::ac
                        val s = String.size next
                        val pad = Vector.sub (widths, col) - s
                        val ac = if pad <= 0
                                    then ac
                                 else (makeBlanks pad)::ac
                     in if col = 0
                           then ac
                        else loop (col - 1,
                                   " "::ac)
                     end
                  val ac = "\n"::ac
               in if width = 0
                     then ac
                  else loop (width - 1, ac)
               end
            val pieces = naturalFold (height,
                                      [],
                                      doRow)
         in String.concat pieces
         end
      end
   end
