(* pingpong.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure PingPong : sig

    val run : int -> unit

  end = struct

    fun run n = let
	  val ch = CML.channel()
	  fun ping i = if (i < n)
		then let
		  val _ = CML.send(ch, i)
		  val ack = CML.recv ch
		  in
		    ping ack
		  end
		else ()
	  fun pong () = let
		val msg = CML.recv ch + 1
		in
		  CML.send (ch, msg);
		  if (msg < n) then pong() else ()
		end
	  in
	    CML.spawn pong;
	    ping 0
	  end

  end
