(* black-scholes.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure BlackScholes : sig

    datatype option_ty = Put | Call

    datatype option_t = Option of {
	spot : real,          (* spot price *)
	strike : real,        (* strike price *)
	interest : real,      (* risk-free interest rate *)
	div_rate : real,      (* dividend rate *)
	volatility : real,    (* volatility *)
	time : real,          (* years to maturity or expiration: 6mos = .5, etc. *)
	opt_type : option_ty,(* Put or Call *)
	div_vals : real,      (* dividend values (not used here) *)
	derivagem : real      (* expected answer from DerivaGem *)
      }

    val dummy_option : option_t

    val readData : string -> option_t list

    val price : option_t -> real

  end = struct

    datatype option_ty = Put | Call

    datatype option_t = Option of {
	spot : real,          (* spot price *)
	strike : real,        (* strike price *)
	interest : real,      (* risk-free interest rate *)
	div_rate : real,      (* dividend rate *)
	volatility : real,    (* volatility *)
	time : real,          (* years to maturity or expiration: 6mos = .5, etc. *)
	opt_type : option_ty,(* Put or Call *)
	div_vals : real,      (* dividend values (not used here) *)
	derivagem : real      (* expected answer from DerivaGem *)
      }

    val dummy_option = Option{
	    spot=123.0, strike=234.0, interest=345.0,
	    div_rate=456.0, volatility=567.0, time=678.0,
	    opt_type=Put, div_vals=789.0,
	    derivagem=890.0
	  }

  (* readData : string -> option_t list *)
  fun readData (infname : string) : option_t list = let
        fun fromString s = Option.valOf (Real.fromString s)
        val inS = TextIO.openIn infname
        val numOptions =
              (Option.valOf o Option.composePartial(Int.fromString, TextIO.inputLine)) inS
        (* string list -> option_t *)
        fun fieldsToOption [
                spot, strike, interest, div_rate, volatility,
                time, opt_type, div_vals, derivagem
              ] = Option {
                  spot=fromString spot,
                  strike=fromString strike,
                  interest=fromString interest,
                  div_rate=fromString div_rate,
                  volatility=fromString volatility,
                  time=fromString time,
                  (* TODO throw exception if neither "P" nor "C" *)
                  opt_type= if opt_type = "P" then Put else Call,
                  div_vals=fromString div_vals,
                  derivagem=fromString derivagem
                }
          | fieldsToOption flds = raise Fail "fieldsToOption: bogus fields"
        val options = let
              fun lp opts = (case TextIO.inputLine inS
                     of NONE => List.rev opts
                      | SOME ln =>
                          lp (fieldsToOption (String.tokens Char.isSpace ln) :: opts)
                    (* end case *))
              in
                lp [] handle ex => (TextIO.closeIn inS; raise ex)
              end
        val _ = TextIO.closeIn inS
        in
          if numOptions = length options
            then options
            else raise Fail "number of options does not match input"
        end

    (* uses Horner's algorithm to evaluate a polynomial whose coefficients are
     * specified in order of ascending degree: x + 2 is [2, 1]
     *)
    fun poly (c::coeffs) (x : real) =
          List.foldl (fn (c, partial) => c + x * partial) c coeffs

    local
      (* probability density function for standard normal distribution:
       * mean = 0, variance = 1 *)
      val normalization_factor = 1.0 / Math.sqrt(2.0 * Math.pi)
      fun std_normal_pdf x = normalization_factor * Math.exp (~0.5 * x * x)

      (* Approximation of the normal cumulative density function for x > 0 which
       * gives error < 7.5e-8. Algorithm 26.3.7 in Abramowitz and Stegun (1964) *)
      (* magic constants specified in order b5, b4, b3, b2, b1, 0.0 *)
      val magic = [
              1.330274429, ~1.821255978, 1.781477937, ~0.356563782,
              0.319381530, 0.0
            ]
      val b0 = 0.2316419
    in
    fun magic_poly x = poly magic (1.0 / (1.0 + b0 * x))
    (* cumulative density function for standard normal distribution *)
    fun std_normal_cdf x = if x < 0.0
          then (std_normal_pdf (~x)) * (magic_poly (~x))
          else 1.0 - (std_normal_pdf x) * (magic_poly x)
    end (* local *)

    fun price opt = let
          val Option{
                  spot, strike, interest, volatility, time, opt_type, derivagem, ...
                } = opt
          val denom = volatility * Math.sqrt (time)
          val strike_exp = strike * Math.exp (~interest * time)
          val log_term = Math.ln (spot / strike)
          val time_term = (interest + (volatility * volatility * 0.5)) * time
          val d1 = (log_term + time_term) / denom
          val d2 = d1 - denom
          val n_d1 = std_normal_cdf d1
          val n_d2 = std_normal_cdf d2
          val retval = (case opt_type
                 of Put => strike_exp * (1.0 - n_d2) - spot * (1.0 - n_d1)
                  | Call => spot * n_d1 - strike_exp * n_d2
                (* end case *))
          in
            Real.abs (retval - derivagem)
	  end

  end
