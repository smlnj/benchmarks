structure Queue :> sig

    type 'a t

    val new : unit -> 'a t

    val next : 'a t -> 'a option

    val insert : 'a t * 'a -> unit

    val isEmpty : 'a t -> bool

  end = struct

    type 'a t = {front : 'a list ref, rear : 'a list ref}

    fun new () = {front = ref [], rear = ref []}

    fun next ({ front, rear } : 'a t) = (case !front
         of [] =>
              (case rev(!rear)
                 of [] => NONE
                  | x::xs => (front := xs; rear := []; SOME x))
          | x::xs => (front := xs; SOME x)
         (* end case *))

    fun insert (q : 'a t, x) = (#rear q) := x :: !(#rear q)

    fun isEmpty (q : 'a t) =
      (case !(#front q)
         of [] => (case !(#rear q) of [] => true | _ => false)
          | _ => false)

  end

structure CML : sig

    val spawn : (unit -> unit) -> unit

    val yield : unit -> unit

    val exit : unit -> 'a

    val run : (unit -> unit) -> unit

    type 'a chan

    val channel : unit -> 'a chan
    val send : 'a chan * 'a -> unit
    val recv : 'a chan -> 'a

  end = struct

    type 'a cont = 'a SMLofNJ.Cont.cont
    val callcc = SMLofNJ.Cont.callcc
    val throw = SMLofNJ.Cont.throw

    (* the scheduling queue *)
    val readyQ : unit cont Queue.t = Queue.new()

    val topCont : unit cont option ref = ref NONE

    fun exit () = (case !topCont
         of SOME k => (topCont := NONE; throw k ())
          | NONE => (
                TextIO.output(TextIO.stdErr, "\n!!! invalid exit\n");
                raise Fail "exit")
          (* end case *))

    (* dispatch the next thread *)
    fun dispatch () = (case Queue.next readyQ
         of NONE => (
              TextIO.output(TextIO.stdErr, "\n!!! deadlock\n");
              exit ())
          | SOME k => throw k ()
          (* end case *))

    fun uncaught exn = (
          TextIO.output(TextIO.stdErr, concat[
              "\n!!! uncaught exception: ", General.exnMessage exn, "\n"
            ]);
          dispatch())

    fun spawn f = callcc (fn (retK : unit cont) =>
            (callcc (fn (thrdK : unit cont) => (
                Queue.insert (readyQ, thrdK); (* insert new thread in readyQ *)
                throw retK ())); (* return to parent *)
            (f ()) handle ex => uncaught ex))

    fun yield () = callcc (fn retK => (
          Queue.insert (readyQ, retK);
          dispatch ()))

    val topCont : unit cont option ref = ref NONE

    fun exit () = (case !topCont
         of SOME k => (topCont := NONE; throw k ())
          | NONE => (
              TextIO.output(TextIO.stdErr, "\n!!! invalid exit\n");
              raise Fail "exit")
          (* end case *))

    fun run f = callcc (fn k => (
          topCont := SOME k;
          (f ()) handle exn => (topCont := NONE; raise exn)))

    (***** Channels *****)

    type 'a chan = {
        sendQ : ('a * unit cont) Queue.t,
        recvQ : 'a cont Queue.t
      }

    fun channel () = {sendQ = Queue.new(), recvQ = Queue.new()}

    fun send (ch : 'a chan, msg : 'a) = callcc (fn retK => (case Queue.next (#recvQ ch)
           of SOME recvK => (
                Queue.insert (readyQ, retK);
                throw recvK msg)
            | NONE => (
                Queue.insert (#sendQ ch, (msg, retK));
                dispatch())
          (* end case *)))

    fun recv (ch : 'a chan) = callcc (fn retK => (case Queue.next (#sendQ ch)
           of SOME(msg, sendK) => (
                Queue.insert (readyQ, sendK);
                msg)
            | NONE => (
                Queue.insert (#recvQ ch, retK);
                dispatch())
          (* end case *)))

  end
