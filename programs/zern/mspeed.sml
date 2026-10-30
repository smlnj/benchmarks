(* test of Zernick phase screen E-field generation *)
(* This is 1.9 times faster than IDL!!!! *)
structure MSpeed =
    struct
	structure F = FastRealArray2

	val sin = Math.sin
	val cos = Math.cos

	val fromInt = LargeReal.fromInt

	(* setup working vectors and arrays *)
	fun collect n f =
	    let
		fun g 0 l = l
		  | g n l = g (n-1) ((f n) :: l)
	    in
		g n nil
	    end

	val ncoefs = 15
	val nx     = 128
	val ny     = nx
	val nel    = nx * ny

	(* generate an array from a scaled vector *)
	fun mulsv (dst, sf, a) =
	    F.unop(dst,a,fn(vsrc) => sf * vsrc)


	(* compute the complex exponential of an array *)
	fun cisv (a, rpart, ipart) =
	    (F.unop(rpart,a,cos);
	     F.unop(ipart,a,sin);
	     (rpart,ipart))

	(* accumulate scaled vectors into an array *)
	fun mpadd dst (sf, src) =
	    F.binop(dst,dst,src,fn(vdst,vsrc) => vdst + sf * vsrc)


	(* compute an E-field from a set of Zernike screens *)
	fun zern (dst, rpart, ipart, coefs, zerns) =
	    (mulsv (dst, hd coefs, hd zerns);
	     ListPair.app (mpadd dst) (tl coefs, tl zerns);
	     cisv (dst, rpart, ipart))

	(* timing tests and reporting *)
	fun report_times(niter, nel, (start, stop)) =
	    let
		val secs = Time.-(stop,start)
		val dur  = Time.toReal(secs) * 1.0E6
		val ops_per_us = ((fromInt niter) * (fromInt nel)) / dur
		val ns_per_op = 1000.0 / ops_per_us
	    in
		(*print(Time.toString (Time.-(stop,start)));
		print("\n");*)
		{ ops_per_us = ops_per_us, ns_per_op = ns_per_op}
	    end

	fun time_iterations f niter =
	    let
		fun iter 0 = Time.now()
		  | iter n = (ignore (f()); iter (n-1))
	    in
		(Time.now(), iter niter)
	    end

	fun ztest niter =
	    report_times(niter, nel,
			 time_iterations
			 (fn () =>
			  let val sum   = F.array(ny,nx, 0.0)
			     val rpart = F.array(ny,nx, 0.0)
			     val ipart = F.array(ny,nx, 0.0)
			     val coefs = collect ncoefs (fn(x) => real(1 + x))
			     val zerns =
				collect ncoefs
				(fn(x) => F.tabulate F.RowMajor
				 (ny, nx, fn(r,c) => 0.01 * real(nx * r + c)))
			     val (rpart, _) =
				zern (sum, rpart, ipart, coefs, zerns)
			  in if Real.abs(FastRealArray2.sub(rpart, 0, 1) - 0.219)
				 < 0.001
				then ()
			     else raise Fail "compiler bug"
			  end)
			 niter)
end
